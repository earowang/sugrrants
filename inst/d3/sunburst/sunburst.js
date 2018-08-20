// https://bl.ocks.org/maybelinot/5552606564ef37b5de7e47ed2b7dc099

var radius = (Math.min(width, height) / 2) - 10;
var x = d3.scaleLinear().range([0, 2 * Math.PI]);
var y = d3.scaleSqrt().range([0, radius]);
var color = d3.scaleOrdinal(d3.schemeCategory20);
var partition = d3.partition();

var arc = d3.arc()
    .startAngle(function(d) {
      return Math.max(0, Math.min(2 * Math.PI, x(d.x0)));
    })
    .endAngle(function(d) {
      return Math.max(0, Math.min(2 * Math.PI, x(d.x1)));
    })
    .innerRadius(function(d) {
      return Math.max(0, y(d.y0));
    })
    .outerRadius(function(d) {
      return Math.max(0, y(d.y1));
    });

var group = svg.append("g")
    .attr(
      "transform",
      "translate(" + width / 2 + "," + height / 2 + ")"
    );

r2d3.onRender(function(data, svg, w, h, options) {
  root = d3.stratify()
    .id(function(d) { return d.name; })
    .parentId(function(d) { return d.parent; })
    (data);
  root.sum(function(d) { return d.size; });
  group.selectAll("path")
    .data(partition(root).descendants())
    .enter().append("path")
    .attr("d", arc)
    .style("fill", function(d) {
      return color((d.children ? d : d.parent).data.name);
    })
    .on("click", click)
    .append("title")
    .text(function(d) { return d.data.name + "\n" + d.value; });
});

function click(d) {
  group
  .transition()
    .duration(750)
    .tween("scale", function() {
      var xd = d3.interpolate(x.domain(), [d.x0, d.x1]),
          yd = d3.interpolate(y.domain(), [d.y0, 1]),
          yr = d3.interpolate(y.range(), [d.y0 ? 20 : 0, radius]);
      return function(t) {
        x.domain(xd(t)); y.domain(yd(t)).range(yr(t));
      };
    })
    .selectAll("path")
      .attrTween("d", function(d) {
        return function() { return arc(d); };
      });
}

