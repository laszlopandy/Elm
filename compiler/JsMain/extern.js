
// consoleLog :: JSString -> IO ()
function consoleLog(text) {
    console.log(text);
}

// installCallback :: JSString -> (JSString -> JSString) -> IO ()
function installCallback(prop, func) {
	window[prop] = function(arg) {
			// Haste values have the form: [0, string]
			return func([0, arg])[1];
		};
}

// getElmDocs :: JSON
function getElmDocs() {
    return toHS(__elmDocs__);
}
