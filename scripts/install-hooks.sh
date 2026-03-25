#!/bin/sh
# Install git hooks for this repository.
# Run once after cloning: ./scripts/install-hooks.sh

ROOT="$(git rev-parse --show-toplevel)"
HOOK="$ROOT/.git/hooks/pre-push"

cat > "$HOOK" << 'HOOKEOF'
#!/bin/sh
# Pre-push hook: verify syntax, compile all code, build Cloud Functions, and run tests.

set -e

ROOT="$(git rev-parse --show-toplevel)"

echo "=== Checking syntax (delimiter balancing) ==="
python3 "$ROOT/scripts/check-syntax.py"

echo ""
echo "=== Compiling Clojure (online client) ==="
cd "$ROOT"
OUTPUT=$(clj -M:compile-online -e "(require 'regicide.online.game-loop 'regicide.online.lobby 'regicide.online.client 'regicide.online.auth)" 2>&1 || true)
if echo "$OUTPUT" | grep -qi "error\|exception"; then
  echo "$OUTPUT"
  echo "Compilation failed."
  exit 1
fi
echo "  OK"

echo ""
echo "=== Building Cloud Functions ==="
cd "$ROOT/functions"
if [ -d "node_modules" ]; then
  OUTPUT=$(npm run build 2>&1)
  echo "$OUTPUT"
  if echo "$OUTPUT" | grep -q "^------ WARNING"; then
    echo "Build produced warnings — failing pre-push."
    exit 1
  fi
else
  echo "Skipping (node_modules not installed — run 'cd functions && npm install' first)"
fi

echo ""
echo "=== Running tests ==="
cd "$ROOT"
clj -M:test
HOOKEOF

chmod +x "$HOOK"
echo "Pre-push hook installed."
