#!/bin/bash

# Base against origin/develop by default, but use pull-request base otherwise
BASE=${BUILDKITE_PULL_REQUEST_BASE_BRANCH:-master}
TAG=$(git tag --points-at HEAD)

[[ -n $TAG ]] && git ls-files && exit

# Finds the commit hash of HEAD of $BASE branch
BASECOMMIT=$(git log origin/$BASE -1 --pretty=format:%H)
# Finds the commit hash of HEAD of the current branch
COMMIT=$(git log -1 --pretty=format:%H)
# Use buildkite commit instead when its defined
[[ -n "$BUILDKITE_COMMIT" ]] && COMMIT=${BUILDKITE_COMMIT}

# Print it to stderr for logging/debugging
>&2 echo "Diffing current commit: ${COMMIT} against commit: ${BASECOMMIT} from branch: ${BASE} ."

# Compare base to the current commit
if [[ $BASECOMMIT != $COMMIT ]]; then
  # Get the files that have diverged from $BASE
  git diff $BASECOMMIT --name-only
else
  if [ -n "${BUILDKITE_INCREMENTAL+x}" ]; then
    # TODO: remove (temporarily install network tooling)
    apt-get install --yes curl jq

    # base DIFF on last successful Buildkite `develop` RUN
    ci_recent_pass_commit=$(
      curl https://graphql.buildkite.com/v1 -H "Authorization: Bearer ${BUILDKITE_API_TOKEN:-$TOKEN}" \
        -d'{"query": "query { pipeline(slug: \"o-1-labs-2/coda\") { builds(first: 1 branch: \"develop\" state: PASSED) { edges { node { commit } } } } }"}' \
      | jq '.data.pipeline.builds.edges[0].node.commit' | tr -d '"'
    )

    git diff "${ci_recent_pass_commit}" --name-only
  else
    # TODO: Dump commits as artifacts when build succeeds so we can diff against
    # that on develop instead of always running all the tests
    git ls-files
  fi
fi
