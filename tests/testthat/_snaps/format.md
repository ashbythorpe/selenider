# Printing works for selenider elements

    Code
      print(element)
    Output
      A selenider element selecting:
      The first element with css selector ".myclass".

---

    Code
      print(element)
    Output
      A selenider element selecting:
      The first element with css selector ".myclass" and xpath ".//p".

---

    Code
      print(element)
    Output
      A selenider element selecting:
      The first element with css selector ".myclass", xpath ".//a", and link text
      "Link".

---

    Code
      print(element)
    Output
      A selenider element selecting:
      * The first element with css selector ".myclass", xpath ".//a", and link text
        "Link".
      * The first child element with css selector ".myclass2".

---

    Code
      print(element)
    Output
      A selenider element selecting:
      * The first element with css selector ".myclass", xpath ".//a", and link text
        "Link".
      * The first child element with css selector ".myclass2" and xpath ".//p".

---

    Code
      print(element)
    Output
      A selenider element selecting:
      * The first element with css selector "a".
      * The first child element with css selector "b".
      * The first child element with css selector "c".

# Printing works for selenider element collections

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass".

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass", xpath ".//a", and link text "Link".

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".myclass", xpath ".//a", and link text
        "Link".
      * The child elements with css selector ".myclass2" and xpath ".//p".

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The first element with css selector "a".
      * The first child element with css selector "b".
      * The child elements with css selector "c".

# Printing filters works

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      The first element with css selector ".myclass".

---

    Code
      print(elements[[5]])
    Output
      A selenider element selecting:
      The 5th element with css selector ".myclass".

---

    Code
      print(elements[5])
    Output
      A collection of selenider elements selecting:
      The 5th elements with css selector ".myclass".

---

    Code
      print(elements[5:10])
    Output
      A collection of selenider elements selecting:
      The 5th, 6th, 7th, 8th, 9th, and 10th elements with css selector ".myclass".

---

    Code
      print(elements[-4])
    Output
      A collection of selenider elements selecting:
      All elements with css selector ".myclass" except the 4th.

---

    Code
      print(elements[-seq_len(6)])
    Output
      A collection of selenider elements selecting:
      All elements with css selector ".myclass" except the 1st, 2nd, 3rd, 4th, 5th,
      and 6th.

---

    Code
      print(elem_filter(elements, is_present))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".myclass" matching the following condition:
        `is_present`

---

    Code
      print(elem_find(elements, is_present))
    Output
      A selenider element selecting:
      * The first element with css selector ".myclass" matching the following
        condition:
        `is_present`

---

    Code
      print(elem_filter(elements, is_present, is_enabled))
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass" matching a custom condition.

---

    Code
      print(elem_find(elements, is_present, is_enabled))
    Output
      A selenider element selecting:
      The first element with css selector ".myclass" matching a custom condition.

---

    Code
      print(elem_filter(elements, is_present)[[4]])
    Output
      A selenider element selecting:
      * The 4th element with css selector ".myclass" matching the following
        condition:
        `is_present`

---

    Code
      print(elem_filter(elements, is_present)[2:6])
    Output
      A collection of selenider elements selecting:
      * The 2nd, 3rd, 4th, 5th, and 6th elements with css selector ".myclass"
        matching the following condition:
        `is_present`

---

    Code
      print(elem_filter(elements, is_present)[-3])
    Output
      A collection of selenider elements selecting:
      * All elements with css selector ".myclass" except the third matching the
        following condition:
        `is_present`

# Printing DOM-relative selectors works

    Code
      print(elem_ancestors(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The ancestors.

---

    Code
      print(elem_ancestors(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first ancestor.

---

    Code
      print(elem_ancestors(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All ancestors except the first.

---

    Code
      print(elem_parent(element))
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The parent of this element.

---

    Code
      print(elem_siblings(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The siblings.

---

    Code
      print(elem_siblings(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first sibling.

---

    Code
      print(elem_siblings(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All siblings except the first.

---

    Code
      print(elem_children(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The direct children.

---

    Code
      print(elem_children(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first direct child.

---

    Code
      print(elem_children(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All direct children except the first.

---

    Code
      print(elem_descendants(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The descendants.

---

    Code
      print(elem_descendants(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first descendant.

---

    Code
      print(elem_descendants(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All descendants except the first.

# Printing flattened objects works

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      A combination of elements.

---

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      The first of a combination of elements.

---

    Code
      print(elements[1:5])
    Output
      A collection of selenider elements selecting:
      The 1st, 2nd, 3rd, 4th, and 5th of a combination of elements.

---

    Code
      print(elements[-3])
    Output
      A collection of selenider elements selecting:
      All elements of a combination of elements except the third.

---

    Code
      print(elem_filter(elements, is_present))
    Output
      A collection of selenider elements selecting:
      * The elements in a combination of elements that match the following condition:
        `is_present`

---

    Code
      print(elem_find(elements, is_present))
    Output
      A selenider element selecting:
      * The first of a combination of elements matching the following condition:
        `is_present`

---

    Code
      print(elem_filter(elements, is_present, is_enabled))
    Output
      A collection of selenider elements selecting:
      The elements in a combination of elements that match a custom condition.

# Printing results of elem_flatmap() works

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".

---

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".
      * The first element.

---

    Code
      print(elements[-1])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".
      * All elements except the first.

---

    Code
      print(elem_filter(elements, is_visible))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".
      * The elements matching the following condition:
        `is_visible`

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The direct children of each element.

---

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first direct child of any element.

---

    Code
      print(elements[-seq_len(10)])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * All direct children of any element except the 1st, 2nd, 3rd, 4th, 5th, 6th,
        7th, 8th, 9th, and 10th.

---

    Code
      print(elem_find(elements, is_visible))
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first direct child of any element matching the following condition:
        `is_visible`

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.

---

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.
      * The first element.

---

    Code
      print(elements[-1])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.
      * All elements except the first.

---

    Code
      print(elem_filter(elements, is_visible))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.
      * The elements matching the following condition:
        `is_visible`

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.

---

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.
      * The first element.

---

    Code
      print(elements[-1])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.
      * All elements except the first.

---

    Code
      print(elem_filter(elements, is_visible))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.
      * The elements matching the following condition:
        `is_visible`

---

    Code
      print(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The direct children of each element.
      * A transformation of each element using `elem_flatmap()`.

---

    Code
      print(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The direct children of any element.
      * A transformation of each element using `elem_flatmap()`.
      * The first element.

---

    Code
      print(elements[-seq_len(10)])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The direct children of any element.
      * A transformation of each element using `elem_flatmap()`.
      * All elements except the 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, and
        10th.

---

    Code
      print(elem_find(elements, is_visible))
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The direct children of any element.
      * A transformation of each element using `elem_flatmap()`.
      * The first element matching the following condition:
        `is_visible`

# Printing works for results of JS expressions.

    Code
      print(js_element)
    Output
      A selenider element selecting:
      The result of a JavaScript expression.

---

    Code
      print(find_element(js_element, "p"))
    Output
      A selenider element selecting:
      * The result of a JavaScript expression.
      * The first child element with css selector "p".

---

    Code
      print(js_elements)
    Output
      A collection of selenider elements selecting:
      The results of a JavaScript expression.

---

    Code
      print(js_elements[1:2])
    Output
      A collection of selenider elements selecting:
      The 1st and 2nd results of a JavaScript expression.

---

    Code
      print(js_elements[[1]])
    Output
      A selenider element selecting:
      The first result of a JavaScript expression.

