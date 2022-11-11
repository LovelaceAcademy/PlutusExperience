export function setHTML (html) {
  return () => {
    document.body.innerHTML = html;
  }
}
