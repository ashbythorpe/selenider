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
      print(ss("*"), width = 80)
    Output
      { selenider_elements (107) }
      [1] <html lang="en"><head>\n<meta http-equiv="Content-Type" content="text/html; c ...
      [2] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\n ...
      [3] <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
      [4] <meta charset="utf-8">
      [5] <meta http-equiv="X-UA-Compatible" content="IE=edge">
      [6] <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to ...
      [7] <meta name="description" content="selenider">
      [8] <title>test-site • selenider</title>
      [9] <script src="../deps/jquery-3.6.0/jquery-3.6.0.min.js"></script>
      [10] <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to ...
      [11] <link href="../deps/bootstrap-5.2.2/bootstrap.min.css" rel="stylesheet">
      [12] <script src="../deps/bootstrap-5.2.2/bootstrap.bundle.min.js"></script>
      [13] <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awes ...
      [14] <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awes ...
      [15] <script src="https://cdn.jsdelivr.net/gh/afeld/bootstrap-toc@v1.0.1/dist/boot ...
      [16] <script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/headroom. ...
      [17] <script src="https://cdnjs.cloudflare.com/ajax/libs/headroom/0.11.0/jQuery.he ...
      [18] <script src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipbo ...
      [19] <script src="https://cdnjs.cloudflare.com/ajax/libs/fuse.js/6.4.6/fuse.js" in ...
      [20] <script src="https://cdnjs.cloudflare.com/ajax/libs/autocomplete.js/0.38.0/au ...
      ...

