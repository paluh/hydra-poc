#!/bin/bash
# SCP files to/from a given instance

# fail if something goes wrong
set -e

FILE_PATH=${1:-'docker/docker-compose.yaml'}
CONNECT_AS=${2:-'ubuntu'}

AWS_PROFILE=$(cat terraform.tfvars | grep -o -P '(?<=profile).*' | grep -o -P '(?<=").*(?=")')
KEY_NAME=$(cat terraform.tfvars | grep -o -P '(?<=key_name).*' | grep -o -P '(?<=").*(?=")')
INSTANCE_TAG="hydraw-$KEY_NAME"
KEY_PAIR_LOCATION="./env/$KEY_NAME.pem"

INSTANCE_DNS=$(aws --profile=$AWS_PROFILE ec2 describe-instances --output json \
  --query 'Reservations[].Instances[].[Tags[?Value==`'$INSTANCE_TAG'`] | [0].Value, State.Name, PublicDnsName]' \
  | jq -c '.[]' | grep running | grep $INSTANCE_TAG | jq '.[2]' | tr -d '"')

scp -i $KEY_PAIR_LOCATION $FILE_PATH $CONNECT_AS@$INSTANCE_DNS:
