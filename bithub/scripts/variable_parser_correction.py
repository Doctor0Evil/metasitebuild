import glob
from ruamel.yaml import YAML
yaml = YAML()
yaml.preserve_quotes = True

for file in glob.glob("**/*.yml", recursive=True) + glob.glob("**/*.yaml", recursive=True):
    try:
        with open(file, "r") as f:
            doc = yaml.load(f)
        # Correction: ensure all env: keys are uppercase with underscores
        changed = False
        def walker(d):
            global changed
            if isinstance(d, dict):
                for k in list(d.keys()):
                    if k == 'env' and isinstance(d[k], dict):
                        new_env = {kk.upper().replace('-', '_'): v for kk, v in d[k].items()}
                        if new_env != d[k]:
                            d[k] = new_env
                            changed = True
                    else:
                        walker(d[k])
            elif isinstance(d, list):
                for item in d:
                    walker(item)
        walker(doc)
        if changed:
            with open(file, "w") as f:
                yaml.dump(doc, f)
    except Exception as e:
        print(f"Variable correction failed for {file}: {e}")
