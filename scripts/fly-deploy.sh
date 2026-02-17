#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP_DIR="$ROOT_DIR/tdf-hq"
CONFIG_FILE="$ROOT_DIR/fly.toml"

cd "$ROOT_DIR"

if ! command -v docker >/dev/null 2>&1; then
  echo "docker CLI is required but not found" >&2
  exit 1
fi

if ! command -v fly >/dev/null 2>&1; then
  echo "fly CLI is required but not found" >&2
  exit 1
fi

COMMIT_SHA=${SOURCE_COMMIT:-$(git rev-parse HEAD)}
IMAGE_REPO=${DOCKER_IMAGE_REPO:-diegueins680/tdf-hq}
IMAGE_TAG=${DOCKER_IMAGE_TAG:-$COMMIT_SHA}
FULL_IMAGE="${IMAGE_REPO}:${IMAGE_TAG}"

export SOURCE_COMMIT="$COMMIT_SHA"
export GIT_SHA="$COMMIT_SHA"

echo "Building ${FULL_IMAGE} from ${APP_DIR}..." >&2
docker buildx build \
  --platform "${DOCKER_PLATFORM:-linux/amd64}" \
  --build-arg SOURCE_COMMIT="$COMMIT_SHA" \
  -f "$APP_DIR/Dockerfile" \
  -t "$FULL_IMAGE" \
  "$ROOT_DIR" \
  --push

echo "Deploying commit $COMMIT_SHA (image ${FULL_IMAGE}) to Fly..." >&2
exec fly deploy \
  --config "$CONFIG_FILE" \
  --image "$FULL_IMAGE" \
  --env SOURCE_COMMIT="$COMMIT_SHA" \
  --env GIT_SHA="$COMMIT_SHA" \
  "$@"
