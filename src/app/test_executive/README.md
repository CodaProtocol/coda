# How to Run Integration Tests


## Prerequisites Environment Setup

Note: this environment setup assumes that one is a member of o(1) labs and has access to organization infrastructure.  You will need an o(1) labs GCP account and AWS account.

1) Download the gcloud integration test API key.  Go to the API Credentials page (https://console.cloud.google.com/apis/credentials), find "Integration-tests log-engine" and copy the key for that onto your clipboard.  Run `export GCLOUD_API_KEY=<key>` and/or put it in one's bashrc or .profile.  Note that this API key is shared by everyone.

2) Download your key file for the `automated-validation` service account.  Go to the IAM Service Accounts page (https://console.cloud.google.com/iam-admin/serviceaccounts), click into the `automated-validation@<email domain>` page, click into the "Keys" section in the topbar, and create a new key (see picture).  Download this key and save to one's preferred path, it will be needed in step 4 of this setup.  Note that each individual should have their own key.

![automated-validation service account "Keys" tab](https://user-images.githubusercontent.com/3465290/112069746-9aaed080-8b29-11eb-83f1-f36876f3ac3d.png)

3) Other than `GCLOUD_API_KEY`, ensure the following other environment variables are also properly set (preferably in in .bashrc or .profile.): 
- `KUBE_CONFIG_PATH`.  this should usually be `~/.kube/config`
- any other vars relating to Google cloud access, 
- any AWS related vars, namely: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_DEFAULT_REGION=us-west-2`, 
- vars relating to ocaml compilation

4) Run the following commands in order to log in to Google Cloud, and activate the service account for one's work machine.

```
gcloud auth login --no-launch-browser <personal login name>
gcloud container clusters get-credentials --region us-west1 mina-integration-west1
kubectl config use-context gke_o1labs-192920_us-west1_mina-integration-west1
gcloud auth activate-service-account <service account name> --key-file=<path to service account key file>
```

When the service account is activated, one can run the integration tests.  However, in the course of using GCP, one may need to re-activate other accounts or set the context to use other clusters, switching away from the service account.  If one is getting authentication errors, then re-running the above commands to set the correct cluster and activate the service account will probably fix them.

5) OPTIONAL: Set the following aliases in one's .bashrc or .bash_aliases (note that aliases don't work if set in .profile):

```
alias test_executive=./_build/default/src/app/test_executive/test_executive.exe
alias logproc=./_build/default/src/app/logproc/logproc.exe
```



## Routine Test Run

1) Go to mina protocol's dockerhub and pick a `coda-daemon-puppeteered` image to run the tests with.  usually, this image should be a recent image on the same branch as one is currently on.

2) Build `test_executive.exe` with the `integration_tests` profile

3) Run `test_executive.exe`, passing in the coda image selected in step 1, and the name of the test one intends to run
  
3.1) It's recommended to run with the `--debug` flag when iterating on the development of tests.  this flag will pause the destruction and cleanup of the generated testnet and associated terraform configuration files, so that those things can be inspected post-hoc
  
3.2) It's also recommended to pipe log output through logproc with a filter to remove Debug and Spam logs be default (those log levels are very verbose and are intended for debugging test framework internals).  Use `tee test.log` to store the raw output into the file `test.log` so that it can be saved and later inspected.

```sh
alias test_executive=./_build/default/src/app/test_executive/test_executive.exe
alias logproc=./_build/default/src/app/logproc/logproc.exe

CODA_IMAGE=... # pick a suitable (recent) "coda-daemon-puppeteered:XXX-develop-XXX" dockerhub
TEST=... # name of the test one wants to run

dune build --profile=integration_tests src/app/test_executive/test_executive.exe src/app/logproc/logproc.exe
test_executive cloud $TEST --coda-image=$CODA_IMAGE --debug | tee test.log | logproc -i inline -f '!(.level in ["Debug", "Spam"])'
```

4) OPTIONAL: In the event that the automatic cleanup doesn't work properly, one needs to do it manually.  Firstly, destroy what's on GCP with `kubectl delete namespace <namespace of test>`.  Then, delete the local testnet directory, which is in `./automation/terraform/testnets/`

## Notes on GCP namespace name

Running the integration test will of course create a testnet on GCP.  In order to differentiate different test runs, a unique testnet namespace is constructed for each testnet.  The namespace is constructed from appending together the first 5 chars of the local system username of the person running the test, the short 7 char git hash, the test name, and part of the timestamp.

format is: `it-{username}-{gitHash}-{testname}`

ex: `it-adalo-3a9f8ce-block-prod`; user is adalovelace, git commit 3a9f8ce, running block production test

GCP namespaces are limited to 53 characters.    This format uses up a fixed minimum of 22 characters, the integration tests will need a further number of those characters when constructing release names, and the longest release name for any resource happens to be "-block-producers" which is another 16 characters. As such the name of an integration test including dashes cannot exceed 15 characters
