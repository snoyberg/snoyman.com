var links = document.links;
for (var i = 0; i < links.length; i++) {
  var link = links[i];
  var isExternalWebLink = /^https?:$/.test(link.protocol) && link.hostname !== window.location.hostname;

  if (isExternalWebLink) {
    link.target = '_blank';
    link.relList.add('noopener');
    link.relList.add('noreferrer');
  }
}
