// Javascript for d3 object for histogram

// Set padding, barWidth and proportional max height of histogram
var barPadding = 5;
var barWidth = Math.floor(width / data.length);
var barHeight = height*0.8;
var barBase = height - 30;

// Create histogram elements
var hist = r2d3.svg.selectAll("rect")
  .data(r2d3.data);
  
// Define histogram column attributes
hist.enter()
  .append("rect")
  .attr("x", function(d, i) { return i * barWidth; })
  .attr("y", function(d) { return barBase - d.prop * barHeight; }) 
  .attr("height", function(d) { return d.prop * barHeight; })
  .attr("width", barWidth - barPadding)
  .attr("fill", "steelblue")
// Turn brown on mouseover
  .on("mouseover", function() {
    d3.select(this)
      .attr("fill", "brown");
  })
// Return to blue when done
  .on("mouseout", function() {
    d3.select(this)
      .attr("fill", "steelblue");
  });
  
hist.exit().remove();


// Create histogram value text
var htext = r2d3.svg.selectAll("text")
  .data(r2d3.data);

// Define text attributes
htext.enter()
  .append("text")
  .text(function(d) { return d.val + d.unit; })
  .attr("x", function(d, i) { return (i * barWidth + (i + 1) * barWidth) / 2; })
  .attr("y", function(d) { return barBase - d.prop * barHeight - 13; })
  .attr("text-anchor", "middle")
  .attr("font-size", "16px")
  .attr("font-weight", "700")
  .attr("fill", "black");
  
htext.enter()
  .append("text")
  .text(function(d) { return d.lab; })
  .attr("x", function(d, i) { return (i * barWidth + (i + 1) * barWidth) / 2; })
  .attr("y", height - 5)
  .attr("text-anchor", "middle")
  .attr("font-size", "16px")
  .attr("font-weight", "700")
  .attr("fill", "black");

//  Create transitions for histogram and text
hist.transition()
  .duration(500)
  .attr("y", function(d) { return barBase - d.prop * barHeight; })
  .attr("height", function(d) { return d.prop * barHeight; });
  
htext.transition()
  .duration(500)
  .text(function(d) { return d.val + d.unit; })
  .attr("y", function(d) {return barBase - d.prop * barHeight - 13});
  
  