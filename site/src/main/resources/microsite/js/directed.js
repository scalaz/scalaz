function directed(elemId, nodes, edges) {

var cy = cytoscape({
  container: document.querySelector(elemId),

  boxSelectionEnabled: false,
  autounselectify: true,
  zoomingEnabled: false,

  style: cytoscape.stylesheet()
    .selector('node')
      .css({
        'content': 'data(name)',
        'text-valign': 'center',
        'color': 'white',
        'text-outline-width': 2,
        'background-color': '#999',
        'text-outline-color': '#999'
      })
    .selector('edge')
      .css({
        'curve-style': 'bezier',
        'target-arrow-shape': 'triangle',
        'target-arrow-color': '#ccc',
        'line-color': '#ccc',
        'width': 1
      })
    .selector(':selected')
      .css({
        'background-color': 'black',
        'line-color': 'black',
        'target-arrow-color': 'black',
        'source-arrow-color': 'black'
      })
    .selector('.faded')
      .css({
        'opacity': 0.25,
        'text-opacity': 0
      }),

  elements: {
    nodes: nodes,
    edges: edges
  },

  layout: {
    name: 'breadthfirst',
    padding: 3,
    fit: false,
    maximalAdjustments: 10,
    directed: true,
    spacingFactor: 1,
    nodeDimensionsIncludeLabels: true
  }
});

cy.on('tap', function(e){
  if( e.cyTarget === cy ){
    cy.elements().removeClass('faded');
  }
})

cy.on('tap', 'node', function() {
  if (this.data('doc')) {
    window.location.href = this.data('doc');
  }
});

};
