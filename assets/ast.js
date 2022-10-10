const HIDE_FIELD = "display";
const VISIBLE_VALUE = "block";
const HIDDEN_VALUE = "none";

document.addEventListener("DOMContentLoaded", function (event) {
});

function click_node(node) {
  let child = node.parentElement.querySelector("#child");
  if (child) {
    if (is_el_visible(child)) {
      child.style[HIDE_FIELD] = HIDDEN_VALUE;
    } else {
      child.style[HIDE_FIELD] = VISIBLE_VALUE;
    }
  }
}

function is_el_visible(el) {
  return el.style[HIDE_FIELD] === VISIBLE_VALUE;
}
