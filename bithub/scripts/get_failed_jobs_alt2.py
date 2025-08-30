import json, sys, os

cache_file = "bithub/cache/failed_jobs_cache.json"
if os.path.exists(cache_file):
    with open(cache_file) as f:
        sys.stdout.write(f.read())
else:
    json.dump([], sys.stdout)
