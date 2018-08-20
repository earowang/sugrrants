// !preview r2d3 data=stratify(tb, ~ country | region | continent, root = "World"), d3_version=4
//
// https://bl.ocks.org/mbostock/4739610f6d96aaad2fb1e78a72b385ab
//
var g = svg.append("g").attr("transform", "translate(" + width / 2 + "," + (height / 2 + 20) + ")");

var stratify = d3.stratify()
    .id(function(d) { return d.name; })
    .parentId(function(d) { return d.parent; });

var cluster = d3.cluster()
    .size([360, width / 2 - 120]);

r2d3.onRender(function(data, svg, w, h, options) {
  var root = stratify(data)
      .sort(function(a, b) { 
        return a.height - b.height || a.id.localeCompare(b.id); 
      });

  cluster(root);

  var link = g.selectAll(".link")
      .data(root.descendants().slice(1))
      .enter().append("path")
      .attr("class", "link")
      .attr("d", function(d) {
        return "M" + project(d.x, d.y)
            + "C" + project(d.x, (d.y + d.parent.y) / 2)
            + " " + project(d.parent.x, (d.y + d.parent.y) / 2)
            + " " + project(d.parent.x, d.parent.y);
      });

  var node = g.selectAll(".node")
      .data(root.descendants())
      .enter().append("g")
      .attr("class", function(d) { 
        return "node" + (d.children ? " node--internal" : " node--leaf"); 
      })
      .attr("transform", function(d) { 
        return "translate(" + project(d.x, d.y) + ")"; 
      });

  node.append("circle")
      .attr("r", 2.5);

  node.append("text")
      .attr("dy", "0.31em")
      .attr("x", function(d) { return d.x < 180 === !d.children ? 6 : -6; })
      .style("text-anchor", function(d) { 
        return d.x < 180 === !d.children ? "start" : "end"; 
      })
      .attr("transform", function(d) { 
        return "rotate(" + (d.x < 180 ? d.x - 90 : d.x + 90) + ")"; 
      })
      .text(function(d) { return d.id.substring(d.id.lastIndexOf(".") + 1); });
});

function project(x, y) {
  var angle = (x - 90) / 180 * Math.PI, radius = y;
  return [radius * Math.cos(angle), radius * Math.sin(angle)];
}

