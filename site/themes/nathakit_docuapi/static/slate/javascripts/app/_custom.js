$(document).ready(function() {

  // on document load - version selector should show current version
  $("#version-selector").val(window.location.pathname.replace(/\//g,''));

  $(".anchor").click( function() {
    event.preventDefault();
    history.replaceState(null, null, $(this).attr("href"));
    $(window).scrollTop($("div[data-unique='"+$(this).attr("name")+"']").offset().top - 49);
  })

  $("#version-selector").change(function() {
    var value = $(this).val();
    var cannonical_url = "http://docs.pupil-labs.com/";
    var hash = window.location.hash;
    window.location.href = cannonical_url+value+hash;
  });

});

document.addEventListener("DOMContentLoaded", function() {

  var elements = document.getElementsByClassName('github-link');

  for (var i = 0; i < elements.length; i++) {
    var _e = elements[i];
    var section_link = _e.getAttribute('data-github');

    while (_e.nextElementSibling && !_e.nextElementSibling.matches('.github-link')) {
      if (_e.matches("h1") || _e.matches("h2") || _e.matches("h3")){
        // the h1,h2,h3 nodes have text and two links as children
        // child_nodes[1] is the first link (hash link to the section)
        // child_nodes[2] or lastChild is the external link to github
        var child_nodes = _e.childNodes;
        var heading_hash = child_nodes[1].getAttribute('href');
        var hash_link = section_link + heading_hash;
        var github_edit_link = _e.lastChild;
        github_edit_link.setAttribute('href',hash_link);
        github_edit_link.insertAdjacentHTML('afterbegin','<i class="material-icons">mode_edit</i>');
      }
      _e = _e.nextElementSibling;
    }

  }

});

