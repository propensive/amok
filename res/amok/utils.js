const iframe = document.getElementById('api');
const body = document.body;
const theme = document.getElementById('theme');
var lastCurrent = null;

function applyTheme(isDark) {
  body.classList.toggle('dark', isDark);

  try {
    const iframeDoc = iframe?.contentDocument || iframe?.contentWindow?.document;
    if (iframeDoc && iframeDoc.body) {
      iframeDoc.body.classList.toggle('dark', isDark);
    }
  } catch (e) {
    console.warn('Could not access iframe for theme update:', e);
  }
}

function getStoredTheme() {
  return localStorage.getItem('theme');
}

function determineInitialTheme() {
  const stored = getStoredTheme();
  if (stored === 'dark') return true;
  if (stored === 'light') return false;
  return window.matchMedia('(prefers-color-scheme: dark)').matches;
}

function setStoredTheme(isDark) {
  localStorage.setItem('theme', isDark ? 'dark' : 'light');
}

const initialThemeIsDark = determineInitialTheme();
applyTheme(initialThemeIsDark);

iframe?.addEventListener('load', () => {
  applyTheme(body.classList.contains('dark'));
});

theme.addEventListener('click', () => {
  const isNowDark = !body.classList.contains('dark');
  applyTheme(isNowDark);
  setStoredTheme(isNowDark);
});

iframe.addEventListener('load', () => {
  const url = iframe.contentWindow.location;
  const detailsId = "menu_"+decodeURIComponent(url.pathname.slice(8));
  const oldLocation = window.location.origin+"/api/";
  history.replaceState(null, '', oldLocation+url.pathname.slice(8));

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
});
