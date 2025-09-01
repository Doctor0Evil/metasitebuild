name: OPA Allowance Check Hybrid Pipeline

on:
  workflow_dispatch:

jobs:
  opa-allow-check:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout sources
        uses: actions/checkout@v4

      - name: Install OPA CLI
        uses: open-policy-agent/setup-opa@v2 # Or latest stable

      - name: OPA Policy Evaluation
        id: opa_eval
        run: |
          DECISION=$(opa eval --data .bithub/policies --input merged-input.json "data.bithub.allow" --format=json | jq -r '.result.expressions.value')
          echo "OPA_DECISION=$DECISION" >> "$GITHUB_OUTPUT"
        shell: bash

      - name: Act on OPA Decision
        run: |
          if [ "${{ steps.opa_eval.outputs.OPA_DECISION }}" != "true" ]; then
            echo "::error::OPA policy denied operation."
            exit 1
          else
            echo "OPA allow decision: permitted."
          fi
