(function () {
  var doc = document.documentElement;
  doc.classList.remove('no-js');
  doc.classList.add('js');

  var links = document.querySelectorAll('a[href^="http"]');
  var host = window.location.hostname;
  for (var i = 0; i < links.length; i += 1) {
    var link = links[i];
    if (link.hostname && link.hostname !== host) {
      link.target = '_blank';
      var rel = link.getAttribute('rel') || '';
      if (rel.indexOf('noopener') === -1) {
        rel = (rel ? rel + ' ' : '') + 'noopener';
      }
      link.setAttribute('rel', rel.trim());
    }
  }
})();
