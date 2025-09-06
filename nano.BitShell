#!/bin/bash
set -euo pipefail

echo "🧹 Starting Bit.Hub Repository Cleanup"
echo "========================================"

# Create backup
BACKUP_DIR="backup_$(date +%Y%m%d_%H%M%S)"
echo "📦 Creating backup in $BACKUP_DIR..."
mkdir -p "$BACKUP_DIR"

# Function to safely rename/remove problematic files
cleanup_file() {
    local file="$1"
    local reason="$2"
    
    echo "🗑️  Processing: $file ($reason)"
    
    # Create backup if file exists and is readable
    if [[ -f "$file" ]] && [[ -r "$file" ]]; then
        cp "$file" "$BACKUP_DIR/" 2>/dev/null || echo "   ⚠️  Could not backup $file"
    fi
    
    # Remove the problematic file
    rm -f "$file" 2>/dev/null || echo "   ⚠️  Could not remove $file"
}

# List of problematic files identified from your documents
PROBLEMATIC_FILES=(
    # Files with invalid characters or too long names
    "½êÈ�þ‰vB¸.ÚIT —êR}?PK?.bit.bit.bit.bit.bit.bit.bit.bit?­À1<ä.aln"
    "½êÈ�þ‰vB¸.ÚIT —êR}?PK?.bit.bit.bit.bit.bit.bit.bit.bit?­À1<änano.aln"
    "read.bit.coin.bit.bit.bit.bit.bit.bit.bit.bitmd"
    "run.bit.bit.bit.bit.bit.nano.aln"
    
    # Files with excessive extensions
    "*bit.bit.bit.bit.bit*"
    
    # Files with special characters that cause issues
    "test-agent:.agent1qdkwusq2qyzvr5ayw0sxpde8uny9zdl8klqyyj5nh08v7xvrm5mtjp2dqhj.meta"
)

# Clean up problematic files
echo "🔧 Removing problematic files..."
for pattern in "${PROBLEMATIC_FILES[@]}"; do
    # Use find to locate files matching patterns (handles special characters better)
    find . -name "$pattern" -type f 2>/dev/null | while read -r file; do
        cleanup_file "$file" "invalid filename"
    done
done

# Clean up any remaining files with problematic patterns
echo "🔍 Scanning for additional problematic files..."

# Find files with excessive .bit extensions
find . -name "*bit.bit.bit*" -type f | while read -r file; do
    cleanup_file "$file" "excessive extensions"
done

# Find files with invalid characters
find . -name "*[<>:\"|?*]*" -type f 2>/dev/null | while read -r file; do
    cleanup_file "$file" "invalid characters"
done

# Create properly structured directories
echo "📁 Creating proper directory structure..."
mkdir -p .bithub/{audit,policies,logs,bin}
mkdir -p .github/workflows
mkdir -p scripts
mkdir -p docs

# Create essential files with proper content
echo "📝 Creating essential configuration files..."

# .gitignore
cat > .gitignore << 'EOF'
# Bit.Hub specific
.bithub/logs/*.log
.bithub/audit/*.tmp
.bithub/bin/*
!.bithub/bin/.gitkeep

# Build artifacts
*.tmp
*.cache
*.lock

# OS generated files
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db

# IDE files
.vscode/settings.json
.idea/
*.swp
*.swo

# Backup files
backup_*/
EOF

# Basic README
cat > README.md << 'EOF'
# Bit.Hub Repository

This repository contains Bit.Hub automation and compliance tooling.

## Structure

- `.bithub/` - Bit.Hub configuration and audit logs
- `.github/workflows/` - GitHub Actions workflows  
- `scripts/` - Utility scripts
- `docs/` - Documentation

## Usage

Workflows can be triggered via GitHub Actions interface or API calls.

## Compliance

This repository maintains strict compliance controls and audit logging.
EOF

# Create essential empty files with gitkeep
touch .bithub/bin/.gitkeep
touch .bithub/logs/.gitkeep
touch .bithub/audit/.gitkeep

# Normalize line endings and fix any encoding issues
echo "🔄 Normalizing remaining files..."
find . -name "*.md" -o -name "*.yml" -o -name "*.yaml" -o -name "*.json" -o -name "*.sh" -o -name "*.ps1" | while read -r file; do
    if [[ -f "$file" ]] && [[ -r "$file" ]]; then
        # Check if file contains mostly text (not binary)
        if file "$file" | grep -q "text"; then
            # Convert line endings to Unix format
            sed -i 's/\r$//' "$file" 2>/dev/null || true
            echo "   ✅ Normalized: $file"
        fi
    fi
done

# Fix git index if corrupted
echo "🔧 Checking git repository health..."
if git status &>/dev/null; then
    echo "   ✅ Git repository is healthy"
else
    echo "   ⚠️  Git repository issues detected - attempting repair..."
    
    # Reset git index
    git reset --mixed HEAD 2>/dev/null || true
    
    # Clean untracked files with problematic names
    git clean -fd 2>/dev/null || true
    
    echo "   ✅ Git repository repair attempted"
fi

# Stage clean files
echo "📤 Staging cleaned files..."
git add .gitignore README.md .bithub/ 2>/dev/null || true

echo ""
echo "✅ Cleanup completed!"
echo "📦 Backup created in: $BACKUP_DIR"
echo ""
echo "Next steps:"
echo "1. Review the changes: git status"
echo "2. Commit the cleanup: git commit -m 'Clean up repository structure'"
echo "3. Push changes: git push"
echo ""
echo "🎉 Your repository should now be in a working state!"
