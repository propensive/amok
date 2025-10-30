const iframe = document.getElementById('api');
const body = document.body;
var lastCurrent = null;

const root = document.getElementById('root');

iframe.addEventListener('load', () => {
  const url = iframe.contentWindow.location;
  const base = url.pathname.indexOf("_entity");
  if (base == -1) {
    return;
  } else {
    const offset = base + 8;
    const details = decodeURIComponent(url.pathname.slice(offset));
    const detailsId = "menu_"+details;
    console.log(detailsId);
    const oldLocation = url.pathname.substring(0, base)+"_api/";
    const newLocation = oldLocation+url.pathname.slice(offset);
    if (details.startsWith(root.value+".")) {
      console.log("same root");
      history.replaceState(null, '', newLocation);
    } else {
      console.log("different root");
      //window.location = newLocation;
    }

    if (!detailsId) return;

    let el = document.getElementById(detailsId);

    if (lastCurrent) { lastCurrent.classList.remove('active'); }
    lastCurrent = el;
    el.classList.add('active');
    while (el && el.tagName === 'DETAILS') {
      el.open = true;
      el = el.parentElement.closest('details');
    }

    document.querySelectorAll('details').forEach(d => {
      if (!d.contains(document.getElementById(detailsId))) { d.open = false; }
    });
  }
});

function getCookie(name) {
  const prefix = encodeURIComponent(name) + '=';
  for (const part of document.cookie.split('; ')) {
    if (part.startsWith(prefix)) return decodeURIComponent(part.slice(prefix.length));
  }
  return null;
}

function setCookie(name, value, { maxAge = 31536000, path = '/', sameSite = 'Lax', secure = location.protocol === 'https:' } = {}) {
  let c = `${encodeURIComponent(name)}=${encodeURIComponent(value || '')}`;
  if (maxAge != null) c += `; Max-Age=${maxAge}`;
  if (path) c += `; Path=${path}`;
  if (sameSite) c += `; SameSite=${sameSite}`;
  if (secure) c += `; Secure`;
  document.cookie = c;
}

function readSet() {
  const raw = getCookie("imports");
  if (!raw) return new Set();
  return new Set(raw.split(',').map(s => s.trim()).filter(Boolean));
}

function writeSet(set) {
  setCookie("imports", Array.from(set).join(','));
}

(function init() {
  if (!root.value) return;
  const set = readSet();
  root.checked = set.has(root.value);
})();

root.addEventListener('change', () => {
  if (!root.value) return;
  const set = readSet();
  if (root.checked) set.add(root.value); else set.delete(root.value);
  writeSet(set);
});
