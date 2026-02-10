//go:build js && wasm

package main

import (
	"bytes"
	"syscall/js"

	"github.com/traefik/yaegi/interp"
	"github.com/traefik/yaegi/stdlib"
)

func runGo(_ js.Value, args []js.Value) any {
	if len(args) < 1 {
		return js.ValueOf(map[string]any{
			"stdout": "",
			"stderr": "",
			"error":  "no code provided",
		})
	}

	code := args[0].String()

	var stdout, stderr bytes.Buffer

	i := interp.New(interp.Options{
		Stdout: &stdout,
		Stderr: &stderr,
	})
	i.Use(stdlib.Symbols)

	_, err := i.Eval(code)

	result := map[string]any{
		"stdout": stdout.String(),
		"stderr": stderr.String(),
	}
	if err != nil {
		result["error"] = err.Error()
	} else {
		result["error"] = ""
	}

	return js.ValueOf(result)
}

func main() {
	js.Global().Set("_yaegiRun", js.FuncOf(runGo))

	// Signal that the runtime is ready
	js.Global().Call("eval", `if (window._onYaegiReady) window._onYaegiReady()`)

	// Keep the program alive
	select {}
}
