// output-loading.js - Toggle loading overlays on -plot / -tabla_detalle outputs based on shiny:recalculating / shiny:value / shiny:error events.

$(document).on('shiny:recalculating', function(e) {
  var id = e.target.id || '';
  // Match the plot and table outputs (namespaced as labor-plot, labor-tabla_detalle)
  if (id.indexOf('-plot') !== -1) {
    var msg = document.getElementById(id.replace('-plot', '-loading_plot_msg'));
    if (msg) msg.classList.add('is-visible');
  }
  if (id.indexOf('-tabla_detalle') !== -1) {
    var msg = document.getElementById(id.replace('-tabla_detalle', '-loading_table_msg'));
    if (msg) msg.classList.add('is-visible');
  }
});

$(document).on('shiny:value shiny:error', function(e) {
  var id = e.target.id || '';
  if (id.indexOf('-plot') !== -1) {
    var msg = document.getElementById(id.replace('-plot', '-loading_plot_msg'));
    if (msg) msg.classList.remove('is-visible');
  }
  if (id.indexOf('-tabla_detalle') !== -1) {
    var msg = document.getElementById(id.replace('-tabla_detalle', '-loading_table_msg'));
    if (msg) msg.classList.remove('is-visible');
  }
});
