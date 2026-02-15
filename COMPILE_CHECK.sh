#!/bin/bash
# Haskell Learning Repository - Compilation Check Script
# This script checks which files compile with base only and which need dependencies

echo "Haskell Learning Repository - Compilation Check"
echo "================================================"
echo ""

# Files that should compile with only base
BASE_ONLY_FILES="hello.hs functions.hs typesystem.hs monads.hs lazyeval.hs realworld.hs exercises.hs patterns.hs"

echo "Files that compile with only base (no external dependencies):"
echo "------------------------------------------------------------"
for file in $BASE_ONLY_FILES; do
    if [ -f "$file" ]; then
        echo -n "Checking $file... "
        if ghc -c "$file" 2>/dev/null; then
            echo "✓"
        else
            echo "✗"
        fi
    fi
done

echo ""
echo "Files that require external dependencies:"
echo "----------------------------------------"
echo "These files demonstrate advanced concepts but need additional packages:"
echo ""
echo "1. quickcheck-properties.hs - Requires: QuickCheck"
echo "   Install: cabal install QuickCheck"
echo ""
echo "2. performance-optimization.hs - Requires: vector, bytestring, text"
echo "   Install: cabal install vector bytestring text"
echo ""
echo "3. haskell-tooling.hs - Reference only (doesn't need compilation)"
echo "   This file contains templates and configuration examples"
echo ""
echo "4. Testing files (testing.hs) - May require QuickCheck for full functionality"
echo ""
echo "To install all dependencies:"
echo "  cabal install QuickCheck vector bytestring text"
echo ""

# Check if we can compile with all dependencies installed
echo "Checking compilation with possible dependencies..."
echo ""
echo "Note: Some files are designed as educational examples and may"
echo "reference external packages. They're meant to be studied rather"
echo "than compiled standalone."
echo ""
echo "To run examples that use external packages, install them first:"
echo "  cabal install --lib QuickCheck vector bytestring text"
echo ""
echo "Or use stack:"
echo "  stack install QuickCheck vector bytestring text"