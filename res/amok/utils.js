const iframe = document.getElementById('api');
const body = document.body;
var lastCurrent = null;

iframe.addEventListener('load', () => {
  const url = iframe.contentWindow.location;
  const base = url.pathname.indexOf("_entity");
  if (base == -1) {
    return;
  } else {
    const offset = base + 8;
    const detailsId = "menu_"+decodeURIComponent(url.pathname.slice(offset));
    console.log(detailsId);
    const oldLocation = url.pathname.substring(0, base)+"_api/";
    history.replaceState(null, '', oldLocation+url.pathname.slice(offset));

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

const toggle = document.getElementById('toggle');

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
  if (!toggle.value) return;
  const set = readSet();
  toggle.checked = set.has(toggle.value);
})();

toggle.addEventListener('change', () => {
  if (!toggle.value) return;
  const set = readSet();
  if (toggle.checked) set.add(toggle.value); else set.delete(toggle.value);
  writeSet(set);
});
