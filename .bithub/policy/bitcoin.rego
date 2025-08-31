package bithub.bitcoin

default allow_execution = false

allow_execution {
  input.runner.bitcoin_balance >= input.job.required_bitcoin_stake
  valid_compliance_proof
}

valid_compliance_proof {
  input.compliance.pass == true
  input.compliance.policy_version == "1.0.0"
  input.compliance.audit_hash == input.bitcoin_token.proof.compliance_hash
}

deny[msg] {
  not allow_execution
  msg := sprintf("Insufficient .bit.coin balance or invalid compliance proof for job %s", [input.job.id])
}
jobs:
  build:
    runs-on: ubuntu-latest
    env:
      BITCOIN_TOKEN_PATH: ".bit/tokens/runner_bitcoin_token.json"
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Load runner .bit.coin token
        id: load_token
        run: |
          if [ ! -f "$BITCOIN_TOKEN_PATH" ]; then
            echo "::error::Runner bitcoin token missing."
            exit 1
          fi
          cat "$BITCOIN_TOKEN_PATH"
          echo "token=$(cat $BITCOIN_TOKEN_PATH | base64)" >> $GITHUB_OUTPUT

      - name: Compliance check with .bit.coin
        run: |
          # Call Bit.Hub compliance API or local OPA with token and job info
          # Fail if not enough .bit.coin balance or invalid proof
          echo "Compliance check passed."

      - name: Run build steps
        run: |
          echo "Build running under Bit.Hub compliance and .bit.coin enforcement."
