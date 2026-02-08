#!/usr/bin/env bash
# Start frontend server

echo "Starting frontend server..."
echo "Choose a server option:"
echo ""
echo "Option 1 - Python (if available):"
echo "  cd frontend && python3 -m http.server 3000"
echo ""
echo "Option 2 - Using nix:"
echo "  cd frontend && nix-shell -p python3 --run 'python3 -m http.server 3000'"
echo ""
echo "Option 3 - Node.js (if available):"
echo "  npx serve frontend -p 3000"
echo ""
echo "Then open: http://localhost:3000/index.html"
echo ""
echo "Make sure the backend is running on port 8080!"

# Try to start with the first available option
if command -v python3 &> /dev/null; then
    cd frontend && python3 -m http.server 3000
elif command -v python &> /dev/null; then
    cd frontend && python -m http.server 3000
elif command -v npx &> /dev/null; then
    npx serve frontend -p 3000
else
    echo "No HTTP server found. Please install python3 or use 'nix-shell -p python3'"
fi
