var ElmRuntime;
(function (ElmRuntime) {
    // Returns boolean indicating if the swap was successful.
    // Requires that the two signal graphs have exactly the same
    // structure.
    function swap(from, to) {
        var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
        if (canSwap) {
            depthFirstTraversals(swapValues, from.inputs, to.inputs);
        }
        from.node.parentNode.replaceChild(to.node, from.node);
        return canSwap;
    }
    ElmRuntime.swap = swap;

    function similar(nodeOld, nodeNew) {
        var idOkay = nodeOld.id === nodeNew.id;
        var lengthOkay = nodeOld.kids.length === nodeNew.kids.length;
        return idOkay && lengthOkay;
    }
    function swapValues(nodeOld, nodeNew) {
        nodeNew.value = nodeOld.value;
        return true;
    }

    // Returns false if the node operation f ever fails.
    function depthFirstTraversals(f, queueOld, queueNew) {
        if (queueOld.length !== queueNew.length)
            return false;
        queueOld = queueOld.slice(0);
        queueNew = queueNew.slice(0);

        var seen = [];
        while (queueOld.length > 0 && queueNew.length > 0) {
            var nodeOld = queueOld.pop();
            var nodeNew = queueNew.pop();
            if (seen.indexOf(nodeOld.id) < 0) {
                if (!f(nodeOld, nodeNew))
                    return false;
                queueOld = queueOld.concat(nodeOld.kids);
                queueNew = queueNew.concat(nodeNew.kids);
                seen.push(nodeOld.id);
            }
        }
        return true;
    }
})(ElmRuntime || (ElmRuntime = {}));
