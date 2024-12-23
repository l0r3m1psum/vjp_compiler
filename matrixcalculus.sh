IFS='' read -r -d '' payload <<'EOF'
{
	"expression": "x'*A*x + c*sin(y)'*x",
	"wrt": {"name": "x", "type": "vector"},
	"varList": [
		{"name": "A", "type": "symmetric matrix"},
		{"name": "c", "type": "scalar"},
		{"name": "x", "type": "vector"},
		{"name": "y", "type": "vector"}
	],
	"n": 4
}
EOF

curl 'https://www.matrixcalculus.org/_show' \
	-s \
	-X 'POST' \
	-H 'Content-Type: application/json' \
	-d  "$payload" | jq -M .derivative
