name: Bit.Hub Compliance Check
on: [push, pull_request]

jobs:
  opa-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install conftest
        run: |
          wget https://github.com/open-policy-agent/conftest/releases/download/v0.45.0/conftest_0.45.0_Linux_x86_64.tar.gz
          tar xzf conftest_*.tar.gz
          sudo mv conftest /usr/local/bin
      - name: Run Bit.Hub Rego Policies
        run: conftest test --policy .bithub/policies .
