var dagre = require("dagre-d3");
var d3 = require("d3");
var Data_Maybe = require("../Data.Maybe/index.js");
var Dagre_Types = require("../Dagre.Types/index.js");

var render = new dagre.render();

exports.setupZoom = function(svg) {
    var el = d3.select(svg).select("svg");
    var inner = el.select("g");
    
    zoom = d3.zoom().on("zoom", function() {
            inner.attr("transform", d3.event.transform);
        });
    el.call(zoom);
}

exports.renderGraph = function(svg, graph) {
    var g = new dagre.graphlib.Graph({multigraph: true}).setGraph({});

    for (var i = 0; i < graph.length; i++) {
        const def = graph[i];
        var props = {};
        const attrs = def instanceof Dagre_Types.Node ? def.value1 : def.value2;

        if (attrs.label instanceof Data_Maybe.Just) 
            props.label = attrs.label.value0;
        if (attrs.cssClass instanceof Data_Maybe.Just) 
            props.class = attrs.cssClass.value0;

        if (def instanceof Dagre_Types.Node) {
            g.setNode(def.value0, props);
        } else if (def instanceof Dagre_Types.Edge) {
            const l = attrs.label instanceof Data_Maybe.Just ? props.label : "";
            g.setEdge(def.value0, def.value1, props, "" + def.value0 + "," + def.value1 + l);
        }
    }
    
    var el = d3.select(svg).select("svg");
    var inner = el.select("g");

    g.graph().transition = function(selection) {
        return selection.transition().duration(500);
    };
    
    inner.call(render, g);

    const w = parseInt(getComputedStyle(el.node()).width, 10);
    const  xCenterOffset = (w - g.graph().width) / 2;
    inner.attr("transform", "translate(" + xCenterOffset + ", 20)");
    el.attr("height", g.graph().height + 40);
}
