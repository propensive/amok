var slides = document.getElementsByTagName('section');
let fragment = window.location.hash;
let match = fragment.match(/^#slide(\d+)$/);
var currentIndex = (match ? parseInt(match[1], 10) : 1) - 1;

scrollToSlide(currentIndex + 1);

(function() {
  const pollingUrl = '/update'; // Replace with your actual URL

  fetch(pollingUrl, { method: 'GET' })
    .then(response => {
      if (response.ok) {
        const baseUrl = window.location.origin + window.location.pathname;
        const uniqueQuery = '?_ts=' + Date.now();
        const overlay = document.getElementById("overlay");
        overlay.classList.add("visible");
        setTimeout(() => {
          window.location.replace(baseUrl + uniqueQuery + "#slide" + (currentIndex + 1));
        }, 300);
      }
    })
    .catch(err => {
      console.error('Polling request failed:', err);
    });
})();

window.addEventListener('load', function() {
  var sections = document.getElementsByTagName('section');

  for (var i = 0; i < sections.length; i++) {
    var section = sections[i];

    var contentHeight = section.scrollHeight;
    var visibleHeight = section.clientHeight;

    if (contentHeight > visibleHeight) {
      var scale = visibleHeight / contentHeight;
      section.style.transform = 'scale(' + scale + ')';
    }
  }

  const overlay = document.getElementById("overlay");
  overlay.classList.remove("visible");

  var items = document.getElementsByTagName('li');
  for (var i = 0; i < items.length; i++) {
    items[i].className += ' hide';
  }
});

function scrollToSlide(index) {
  if (index < 0 || index >= slides.length) return;
  currentIndex = index;
  var slide = slides[currentIndex];
  slide.scrollIntoView({ behavior: 'smooth', block: 'start' });
  if (slide.id) {
    history.replaceState(null, '', '#' + slide.id);
  }
}

function getVisibleRadioGroup(slide) {
  var inputs = slide.getElementsByTagName('input');
  var radios = [];
  var name = null;

  // Find the first radio button and get its name
  for (var i = 0; i < inputs.length; i++) {
    if (inputs[i].type === 'radio') {
      name = inputs[i].name;
      break;
    }
  }

  if (!name) return null;

  // Collect all radios with that name
  for (var j = 0; j < inputs.length; j++) {
    if (inputs[j].type === 'radio' && inputs[j].name === name) {
      radios.push(inputs[j]);
    }
  }

  return radios.length > 1 ? radios : null;
}

function advanceRadioOption(slide) {
  var radios = getVisibleRadioGroup(slide);
  if (!radios) return false;

  var selectedIndex = -1;
  for (var i = 0; i < radios.length; i++) {
    if (radios[i].checked) {
      selectedIndex = i;
      break;
    }
  }

  if (selectedIndex === -1) {
    radios[0].checked = true;
    return true;
  } else if (selectedIndex < radios.length - 1) {
    radios[selectedIndex + 1].checked = true;
    return true;
  }

  return false; // Already at last radio
}

function retreatRadioOption(slide) {
  var radios = getVisibleRadioGroup(slide);
  if (!radios) return false;

  var selectedIndex = -1;
  for (var i = 0; i < radios.length; i++) {
    if (radios[i].checked) {
      selectedIndex = i;
      break;
    }
  }

  if (selectedIndex > 0) {
    radios[selectedIndex - 1].checked = true;
    return true;
  }

  return false;
}

document.addEventListener('keydown', function(e) {
  var key = e.key || e.keyCode;
  var slide = slides[currentIndex];

  if (key === 'ArrowRight' || key === 39) {
    if (advanceRadioOption(slide)) return;
    if (revealNextListItem(slide)) return;
    scrollToSlide(currentIndex + 1);
  }

  if (key === 'ArrowLeft' || key === 37) {
    if (retreatRadioOption(slide)) return;
    if (hideLastListItem(slide)) return;
    scrollToSlide(currentIndex - 1);
  }

  if (key === 'Enter' || key === 10 || key === 13) {
    document.documentElement.requestFullscreen();
  }
});

function revealNextListItem(slide) {
  var lists = slide.getElementsByTagName('ul');
  if (lists.length === 0) return false;

  for (var i = 0; i < lists.length; i++) {
    var items = lists[i].getElementsByTagName('li');
    for (var j = 0; j < items.length; j++) {
      if (items[j].className.indexOf('hide') !== -1) {
        items[j].className = items[j].className.replace(/\bhide\b/, '').trim();
        return true;
      }
    }
  }

  return false; // nothing left to reveal
}

function hideLastListItem(slide) {
  var lists = slide.getElementsByTagName('ul');
  if (lists.length === 0) return false;

  for (var i = lists.length - 1; i >= 0; i--) {
    var items = lists[i].getElementsByTagName('li');
    for (var j = items.length - 1; j >= 0; j--) {
      if (items[j].className.indexOf('hide') === -1) {
        items[j].className += ' hide';
        return true; // hid one
      }
    }
  }

  return false; // nothing visible to hide
}
