#!/usr/bin/env python3

import pytz
import datetime
import timedelta
import json
import re
import subprocess
import sys

from kubernetes import client, config
import click

@click.group()
@click.option('--debug/--no-debug', default=False)
def cli(debug):
    pass


@cli.group()
def janitor():
    pass


@cli.group()
def watchdog():
    pass

###
# Commands for Testnet kubernetes resource cleanup
###


DEFAULT_K8S_CONFIG_PATH = "kube-config"
DEFAULT_CLEANUP_THRESHOLD = (60 * 60 * 24 * 7)  # 7 days

DEFAULT_CLEANUP_PATTERNS = []
DEFAULT_K8S_CONTEXTS = []


def execute_command(command):
    process = subprocess.Popen(command.split(), stdout=subprocess.PIPE)
    output, error = process.communicate()

    if process.returncode > 0:
        print(output.decode('utf-8'))
        print(error.decode('utf-8'))
        print('Executing command \"%s\" returned a non-zero status code %d' % (command, process.returncode))
        sys.exit(process.returncode)

    if error:
        print(error.decode('utf-8'))

    return output.decode('utf-8')

def get_all_namespaces():
    items = json.loads(execute_command('kubectl get namespaces -ojson'))['items']
    return [item['metadata']['name'] for item in items]

def get_active_node_ports():
    ports = set()
    for namespace in get_all_namespaces():
        result = json.loads(execute_command('kubectl -n %s get services -ojson' % namespace))
        for service in result['items']:
            if service['spec']['type'] == 'NodePort':
                for port in service['spec']['ports']:
                    ports.add(port['nodePort'])

    return ports

def get_mapped_ports(instance_group, zone):
    results = execute_command(
        'gcloud compute instance-groups get-named-ports %s --zone=%s'
        % (instance_group, zone)
    ).splitlines()[1:]

    mappings = {}
    for result in results:
        [name, port] = re.sub(' +', ' ', result).split(' ')
        mappings[name] = int(port)

    return mappings

def set_port_mappings(instance_group, zone, port_mappings):
    assignments = ['%s:%d' % (name, port) for name, port in port_mappings.items()]
    print(execute_command(
        'gcloud compute instance-groups set-named-ports %s --zone=%s --named-ports=%s'
        % (instance_group, zone, ','.join(assignments))
    ))

@janitor.command()
@click.option('--instance-group',
              required=True,
              help='Instance group to cleanup port mappings for')
@click.option('--zone',
              required=True,
              multiple=True,
              help='Zone to cleanup port mappings in')
@click.option('--k8s-context',
              required=True,
              help='Kubernetes cluster context to scan for actively used port mappings')
@click.option('--kube-config-file',
              required=True,
              help='Path to load Kubernetes config from')
def cleanup_port_mappings(instance_group, zone, k8s_context, kube_config_file):

    config.load_kube_config(context=k8s_context, config_file=kube_config_file)
    print(execute_command("kubectl config use-context %s" % k8s_context))

    print('Finding active node port services')
    active_ports = get_active_node_ports()

    for z in zone:
        print('Pruning port mappings from %s' % z)
        existing_mappings = get_mapped_ports(instance_group, z)
        active_mappings = {name: port for name, port in existing_mappings.items() if port in active_ports}
        set_port_mappings(instance_group, z, active_mappings)
        print('Pruned %d port mappings from %s' % (len(existing_mappings) - len(active_mappings), z))

@janitor.command()
@click.option('--namespace-pattern',
              multiple=True,
              default=DEFAULT_CLEANUP_PATTERNS,
              help='regular expression expressing a list of namespace patterns to include in the cleanup operation')
@click.option('--cleanup-older-than',
              default=DEFAULT_CLEANUP_THRESHOLD,
              help='threshold in seconds over which to include discovered namespaces in cleanup')
@click.option('--k8s-context',
              multiple=True,
              default=DEFAULT_K8S_CONTEXTS,
              help='Kubernetes cluster context to scan for namespaces to cleanup')
@click.option('--kube-config-file',
              default=DEFAULT_K8S_CONFIG_PATH,
              help='Path to load Kubernetes config from')
def cleanup_namespace_resources(namespace_pattern, cleanup_older_than, k8s_context, kube_config_file):
    """Delete resources within target namespaces which have exceeded some AGE threshold."""

    for ctx in k8s_context:
        print("Processing Kubernetes context {context}...".format(context=ctx))
        execute_command("kubectl config use-context {context}".format(context=ctx))

        config.load_kube_config(context=ctx, config_file=kube_config_file)
        v1 = client.CoreV1Api()
        response = v1.list_namespace()
        for pattern in namespace_pattern:
            print(
                "Checking cluster namespaces against pattern: {p}".format(p=pattern))

            regexp = re.compile(pattern)
            for ns in response.items:
                print("Namespace: {namespace}, creation_time: {createdAt}, age: {age}".format(
                    namespace=ns.metadata.name,
                    createdAt=ns.metadata.creation_timestamp,
                    age=str(datetime.datetime.now(pytz.utc) - ns.metadata.creation_timestamp))
                )

                # cleanup all namespace resources which match pattern and whose creation time is older than threshold
                if ns.metadata.name and regexp.search(ns.metadata.name) \
                    and (ns.metadata.creation_timestamp <= datetime.datetime.now(pytz.utc) - datetime.timedelta(seconds=cleanup_older_than)):

                    print("Namespace [{namespace}] exceeds age of {age} seconds. Cleaning up resources...".format(
                        namespace=ns.metadata.name, age=cleanup_older_than))

                    execute_command("kubectl delete all -n {namespace} --all".format(namespace=ns.metadata.name))
                    execute_command("kubectl delete ns {namespace}".format(namespace=ns.metadata.name))
                else:
                    print("Skipping Namespace {namespace}.".format(
                        namespace=ns.metadata.name))


if __name__ == "__main__":
    cli()
