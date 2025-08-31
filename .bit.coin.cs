var meta = JsonDocument.Parse(File.ReadAllText(".meta/hyper.parameters.json")).RootElement;
var bitCoinCfg = meta.GetProperty("bitCoin");
var tokenPath = bitCoinCfg.GetProperty("tokenPath").GetString();
if (File.Exists(tokenPath))
{
    var token = JsonDocument.Parse(File.ReadAllText(tokenPath)).RootElement;
    var compscore = token.GetProperty("compscore").GetInt32();

    var yieldPolicy = bitCoinCfg.GetProperty("yieldPolicy");
    if (compscore < 800)
    {
        ApplyYield(yieldPolicy.GetProperty("compscoreLow"));
    }
    if (compscore < 600)
    {
        ApplyYield(yieldPolicy.GetProperty("compscoreCritical"));
    }
}

void ApplyYield(JsonElement policy)
{
    var yieldPercent = policy.GetProperty("yieldPercent").GetInt32();
    Console.WriteLine($"[Bit.Hub] Yielding {yieldPercent}% workload due to .bit.coin policy");
    // Example: reduce thread pool
    ThreadPool.GetMaxThreads(out var worker, out var io);
    ThreadPool.SetMaxThreads(worker * (100 - yieldPercent) / 100, io);
    // Optional: log to audit
    File.AppendAllText(bitCoinCfg.GetProperty("failSafes").GetProperty("auditLog").GetString(),
        $"{DateTime.UtcNow:o} yield={yieldPercent}%\n");
}
