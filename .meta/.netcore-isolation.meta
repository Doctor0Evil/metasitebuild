using System.Text.Json;

var metaPath = Path.Combine(Directory.GetCurrentDirectory(), ".meta", "hyper.parameters.json");
if (File.Exists(metaPath))
{
    var json = File.ReadAllText(metaPath);
    var cfg = JsonDocument.Parse(json).RootElement;

    var maxThreads = cfg.GetProperty("runtime").GetProperty("maxThreads").GetInt32();
    ThreadPool.SetMaxThreads(maxThreads, maxThreads);

    var memLimit = cfg.GetProperty("runtime").GetProperty("memoryLimitMB").GetInt32();
    Console.WriteLine($"[Bit.Hub] Applying memory limit: {memLimit} MB");

    // …apply other knobs as needed
}
else
{
    Console.WriteLine("[Bit.Hub] No hyper.parameters.json found; using defaults.");
}
