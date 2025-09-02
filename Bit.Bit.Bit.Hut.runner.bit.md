
Settings: Doctor0Evil/Bit.Hub
Access
Code and automation
Security
Integrations
Runners / Add new self-hosted runner · Doctor0Evil/Bit.Hub
Adding a self-hosted runner requires that you download, configure, and execute the GitHub Actions Runner. If you do not already have an existing volume licensing agreement for your GitHub purchases, by downloading and configuring the GitHub Actions Runner, you agree to the GitHub Customer Agreement.

Runner image

macOS

Linux

Windows
Architecture
Download
We recommend configuring the runner under "\actions-runner". This will help avoid issues related to service identity folder permissions and long path restrictions on Windows.

# Create a folder under the drive root
$ mkdir actions-runner; cd actions-runner# Download the latest runner package
$ Invoke-WebRequest -Uri https://github.com/actions/runner/releases/download/v2.328.0/actions-runner-win-x64-2.328.0.zip -OutFile actions-runner-win-x64-2.328.0.zip# Optional: Validate the hash
$ if((Get-FileHash -Path actions-runner-win-x64-2.328.0.zip -Algorithm SHA256).Hash.ToUpper() -ne 'a73ae192b8b2b782e1d90c08923030930b0b96ed394fe56413a073cc6f694877'.ToUpper()){ throw 'Computed checksum did not match' }# Extract the installer
$ Add-Type -AssemblyName System.IO.Compression.FileSystem ; [System.IO.Compression.ZipFile]::ExtractToDirectory("$PWD/actions-runner-win-x64-2.328.0.zip", "$PWD")
Configure
# Create the runner and start the configuration experience
$ ./config.cmd --url https://github.com/Doctor0Evil/Bit.Hub --token BT3OJSIX2FTOFX4DPQ7TWFDIW2KQG# Run it!
$ ./run.cmd
Using your self-hosted runner
# Use this YAML in your workflow file for each job
runs-on: self-hosted
For additional details about configuring, running, or shutting down the runner, please check out our product docs.

Footer
© 2025 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact
Manage cookies
Do not share my personal information
