const iframe = document.getElementById('api');
const body = document.body;
const theme = document.getElementById('theme');

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
  console.log("menu_"+url.pathname.slice(8));
  const detailsId = "menu_"+url.pathname.slice(8);

  // Your logic to compute the matching <details> ID from the URL
  if (!detailsId) return;

  // Open the matching <details> and all ancestors
  let el = document.getElementById(detailsId);
  while (el && el.tagName === 'DETAILS') {
    el.open = true;
    el = el.parentElement.closest('details');
  }

  // Optional: close unrelated <details> sections
  closeOtherDetails(detailsId);
});

function computeMenuIdFromUrl(url) {
  // Example: match path to a known ID
  const path = new URL(url).pathname;
  const map = {
    '/a1.html': 'menu-a1',
    '/b.html': 'menu-b',
  };
  return map[path];
}

function closeOtherDetails(keepId) {
  document.querySelectorAll('details').forEach(d => {
    if (!d.contains(document.getElementById(keepId))) {
      d.open = false;
    }
  });
}
