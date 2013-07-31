
// consoleLog :: JSString -> IO ()
function consoleLog(text) {
    console.log(text);
    parent.frames[2].document.body.innerHTML = text;
}

// getElmDocs :: JSON
function getElmDocs() {
    return toHS(__elmDocs__);
}
