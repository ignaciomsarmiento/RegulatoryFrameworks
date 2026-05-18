// header.js - Custom Shiny message handlers for trigger-download and header-active topic state.

Shiny.addCustomMessageHandler('trigger-download', function(id) {
  var el = document.getElementById(id);
  if (el) el.click();
});

window.setHeaderTopic = function(activeTopic) {
  document.querySelectorAll('.header-topic-link').forEach(function(link) {
    link.classList.remove('active');
  });

  var activeLink = document.querySelector(
    '.header-topic-link[data-topic="' + activeTopic + '"]'
  );
  if (activeLink) {
    activeLink.classList.add('active');
  }

  var header = document.querySelector('.header');
  if (header) {
    var hideNavOnLanding = activeTopic === 'landing' && !window.showHeaderNavOnLanding;
    header.classList.toggle('header-nav-hidden-on-landing', hideNavOnLanding);
  }
};

function registerHeaderActiveHandler() {
  if (!window.Shiny || window.headerActiveHandlerRegistered) return;
  Shiny.addCustomMessageHandler('header-active', function(topic) {
    window.setHeaderTopic(topic);
  });
  window.headerActiveHandlerRegistered = true;
}

var headerActiveHandlerAttempts = 0;
function ensureHeaderActiveHandler() {
  registerHeaderActiveHandler();
  if (!window.headerActiveHandlerRegistered && headerActiveHandlerAttempts < 40) {
    headerActiveHandlerAttempts += 1;
    window.setTimeout(ensureHeaderActiveHandler, 50);
  }
}

document.addEventListener('DOMContentLoaded', function() {
  window.setHeaderTopic('landing');
  ensureHeaderActiveHandler();
});
