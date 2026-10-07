#!/bin/sh
set -eu

# All five reminders were sent and verified ahead of schedule on September 14.
# Retain a harmless entrypoint so an old queued invocation cannot repeat them.
echo 'No reminders sent: this batch was completed on 2026-09-14 at 19:24 America/Guayaquil.'
