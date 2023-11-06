# Printing elements works

    Code
      print(s(".toggleable"), width = 80)
    Output
      { selenider_element }
      <div class="toggleable" style="display: none;">
        \n<p>Hello!</p>\n
      </div>

---

    Code
      print(s("#toggle_div"), width = 80)
    Output
      { selenider_element }
      <button id="toggle_div">
        Toggle div
      </button>

---

    Code
      print(elem_children(s(".actions-test")), width = 80)
    Output
      { selenider_elements (5) }
      [1] <button type="button" class="actions-button">Test button</button>
      [2] <p id="button-output"></p>
      [3] <input type="text" class="actions-input">
      [4] <p id="text-output"></p>
      [5] <input type="submit" value="Submit">

---

    Code
      print(elem_children(s(".actions-test")), width = 80, n = 3)
    Output
      { selenider_elements (5) }
      [1] <button type="button" class="actions-button">Test button</button>
      [2] <p id="button-output"></p>
      [3] <input type="text" class="actions-input">
      ...

