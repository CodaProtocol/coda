#!/bin/bash
set -eo pipefail

set +x
echo "Exporting Variables: "

export GITHASH=$(git rev-parse --short=7 HEAD)
export GITBRANCH=$(git rev-parse --symbolic-full-name --abbrev-ref HEAD |  sed 's!/!-!g; s!_!-!g' )
# GITTAG is the closest tagged commit to this commit, while THIS_COMMIT_TAG only has a value when the current commit is tagged
export GITTAG=$(git describe --always --abbrev=0 | sed 's!/!-!g; s!_!-!g')
export THIS_COMMIT_TAG=$(git tag --points-at HEAD)
export PROJECT="mina"

set +u
export BUILD_NUM=${BUILDKITE_BUILD_NUM}
export BUILD_URL=${BUILDKITE_BUILD_URL}
set -u

export MINA_DEB_CODENAME=${MINA_DEB_CODENAME:=stretch}

[[ -n "$BUILDKITE_BRANCH" ]] && export GITBRANCH=$(echo "$BUILDKITE_BRANCH" | sed 's!/!-!g; s!_!-!g')

if [[ -n "${THIS_COMMIT_TAG}" ]]; then # If the commit is tagged
    export MINA_DEB_VERSION="${GITTAG}-${GITHASH}"
    export MINA_DOCKER_TAG="$(echo "${MINA_DEB_VERSION}" | sed 's!/!-!g; s!_!-!g')-${MINA_DEB_CODENAME}"
else
    export MINA_DEB_VERSION="${GITTAG}-${GITBRANCH}-${GITHASH}"
    export MINA_DOCKER_TAG="$(echo "${MINA_DEB_VERSION}" | sed 's!/!-!g; s!_!-!g')-${MINA_DEB_CODENAME}"
fi

# Determine deb repo to use
case $GITBRANCH in
    master)
        RELEASE=stable ;;
    compatible|master|release*) # whitelist of branches that can be tagged
        case "${THIS_COMMIT_TAG}" in
          *alpha*) # any tag including the string `alpha`
            RELEASE=alpha ;;
          *beta*) # any tag including the string `beta`
            RELEASE=beta ;;
          ?*) # Any other non-empty tag. ? matches a single character and * matches 0 or more characters.
            RELEASE=stable ;;
          "") # No tag
            RELEASE=unstable ;;
          *) # The above set of cases should be exhaustive, if they're not then still set RELEASE=unstable
            RELEASE=unstable
            echo "git tag --points-at HEAD may have failed, falling back to unstable. Value: \"$(git tag --points-at HEAD)\""
            ;;
        esac ;;
    *)
        RELEASE=unstable ;;
esac

echo "Publishing on release channel \"${RELEASE}\" based on branch \"${GITBRANCH}\" and tag \"${THIS_COMMIT_TAG}\""
[[ -n ${THIS_COMMIT_TAG} ]] && export MINA_COMMIT_TAG="${THIS_COMMIT_TAG}"
export MINA_DEB_RELEASE="${RELEASE}"

set -x
