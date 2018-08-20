// !preview r2d3 data=stratify(tb, ~ country | region | continent, root = "World"), d3_version=4
//
// https://bl.ocks.org/mbostock/4063530
//
var g = svg.append("g").attr("transform", "translate(2,2)"),
    format = d3.format(",d");

var pack = d3.pack()
    .size([width - 4, width - 4]);

r2d3.onRender(function(root, svg, w, h, options) {
  root = d3.stratify()
    .id(function(d) { return d.name; })
    .parentId(function(d) { return d.parent; })
    (data)
    .sum(function(d) { return d.size; })
    .sort(function(a, b) { return b.value - a.value; });

  var node = g.selectAll(".node")
    .data(pack(root).descendants())
    .enter().append("g")
      .attr("class", function(d) { return d.children ? "node" : "leaf node"; })
      .attr("transform", function(d) { 
        return "translate(" + d.x + "," + d.y + ")"; 
      });

  node.append("title")
      .text(function(d) { return d.data.name + "\n" + format(d.value); });

  node.append("circle")
      .attr("r", function(d) { return d.r; });

  node.filter(function(d) { return !d.children; }).append("text")
      .attr("dy", "0.3em")
      .text(function(d) { return d.data.name.substring(0, d.r / 3); });
});
