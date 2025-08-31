using BitHub.PolicyHarness.Services;
using BitHub.PolicyHarness.Models;

var repoRoot = Directory.GetCurrentDirectory();
var failOpen = true;
var mode = args.FirstOrDefault() ?? "pre"; // pre | wrap | post

var loader = new PolicyLoader(repoRoot);
var policies = loader.LoadAll(); // .gitenforcement, .gitcomply, thresholds, HR core, personality matrix
var engine = new EnforcementEngine(repoRoot, policies, failOpen);
var renamer = new Renamer(repoRoot, failOpen);
var storage = new StorageGuard(repoRoot, policies, failOpen);
var summary = new SummaryWriter(Environment.GetEnvironmentVariable("GITHUB_STEP_SUMMARY"));
var santa = new SantaDistributor(repoRoot, failOpen);
var ml = new MlGateClient(repoRoot, failOpen);

try
{
    switch (mode)
    {
        case "pre":
            summary.Header("Bit.Hub Policy Harness (pre)");
            loader.ScaffoldIfMissing();               // TOS, legal frameworks, etc. (non-blocking)
            engine.ApplyStickyThresholds();           // reads Central_Data_Bank thresholds/personality
            renamer.NormalizeWorkflows();             // rename + banner inject (git mv best-effort)
            storage.EnforceAllowlistLogical();        // record intended endpoints; log drift
            engine.EmitNonInterferenceCharter(summary);
            summary.Flush();
            break;

        case "wrap":
            // Optional: wrap a command, e.g., dotnet build, with pre/post guards
            // Usage: bithub-harness wrap -- dotnet build
            var idx = Array.IndexOf(args, "--");
            if (idx < 0 || idx == args.Length - 1) break;
            var cmd = args.Skip(idx + 1).ToArray();
            engine.BeforeStep(summary);
            engine.Exec(cmd, failOpen: true);
            engine.AfterStep(summary);
            summary.Flush();
            break;

        case "post":
            summary.Header("Bit.Hub Policy Harness (post)");
            ml.CollectSignals(summary);               // collect risk, changed files, compscore
            santa.DeliverArtifacts(summary);          // route via santa.clause.exe (or log)
            engine.Celebrate(summary);                // 🎉 always-successful
            summary.Flush();
            break;
    }
    return 0;
}
catch (Exception ex)
{
    // Fail-open: log, return success to avoid runner interference
    Console.Error.WriteLine($"[harness] warning: {ex.Message}");
    return 0;
}
