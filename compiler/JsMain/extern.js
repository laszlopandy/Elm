
// consoleLog :: JSString -> IO ()
function consoleLog(text) {
    console.log(text);
}

// installCallback :: JSString -> (JSString -> JSString) -> IO ()
function installCallback(prop, func) {
	window[prop] = func;
}

// getElmDocs :: JSON
function getElmDocs() {
    return toHS(__elmDocs__);
}
