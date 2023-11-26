# Lazy printing works for selenider elements

    Code
      print_lazy(element)
    Output
      A selenider element selecting:
      The first element with css selector ".myclass".

---

    Code
      print_lazy(element)
    Output
      A selenider element selecting:
      The first element with css selector ".myclass" and xpath ".//p".

---

    Code
      print_lazy(element)
    Output
      A selenider element selecting:
      The first element with css selector ".myclass", xpath ".//a", and name "name".

---

    Code
      print_lazy(element)
    Output
      A selenider element selecting:
      * The first element with css selector ".myclass", xpath ".//a", and name
        "name".
      * The first child element with css selector ".myclass2".

---

    Code
      print_lazy(element)
    Output
      A selenider element selecting:
      * The first element with css selector ".myclass", xpath ".//a", and name
        "name".
      * The first child element with css selector ".myclass2" and xpath ".//p".

---

    Code
      print_lazy(element)
    Output
      A selenider element selecting:
      * The first element with css selector "a".
      * The first child element with css selector "b".
      * The first child element with css selector "c".

# Lazy printing works for selenider element collections

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass".

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass", xpath ".//a", and name "name".

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".myclass", xpath ".//a", and name
        "name".
      * The child elements with css selector ".myclass2" and xpath ".//p".

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The first element with css selector "a".
      * The first child element with css selector "b".
      * The child elements with css selector "c".

# Lazy printing filters works

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      The first element with css selector ".myclass".

---

    Code
      print_lazy(elements[[5]])
    Output
      A selenider element selecting:
      The 5th element with css selector ".myclass".

---

    Code
      print_lazy(elements[5])
    Output
      A collection of selenider elements selecting:
      The 5th element with css selector ".myclass".

---

    Code
      print_lazy(elements[5:10])
    Output
      A collection of selenider elements selecting:
      The 5th, 6th, 7th, 8th, 9th, and 10th elements with css selector ".myclass".

---

    Code
      print_lazy(elements[-4])
    Output
      A collection of selenider elements selecting:
      All elements with css selector ".myclass" except the 4th.

---

    Code
      print_lazy(elements[-seq_len(6)])
    Output
      A collection of selenider elements selecting:
      All elements with css selector ".myclass" except the 1st, 2nd, 3rd, 4th, 5th,
      and 6th.

---

    Code
      print_lazy(elem_filter(elements, is_present))
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass" matching the following
      condition:`is_present`

---

    Code
      print_lazy(elem_find(elements, is_present))
    Output
      A selenider element selecting:
      The first element with css selector ".myclass" matching the following
      condition:`is_present`

---

    Code
      print_lazy(elem_filter(elements, is_present, is_enabled))
    Output
      A collection of selenider elements selecting:
      The elements with css selector ".myclass" matching a custom condition.

---

    Code
      print_lazy(elem_find(elements, is_present, is_enabled))
    Output
      A selenider element selecting:
      The first element with css selector ".myclass" matching a custom condition.

---

    Code
      print_lazy(elem_filter(elements, is_present)[[4]])
    Output
      A selenider element selecting:
      The 4th element with css selector ".myclass" matching the following
      condition:`is_present`

---

    Code
      print_lazy(elem_filter(elements, is_present)[2:6])
    Output
      A collection of selenider elements selecting:
      The 2nd, 3rd, 4th, 5th, and 6th elements with css selector ".myclass" matching
      the following condition:`is_present`

---

    Code
      print_lazy(elem_filter(elements, is_present)[-3])
    Output
      A collection of selenider elements selecting:
      All elements with css selector ".myclass" except the third matching the
      following condition:`is_present`

# Lazy printing DOM-relative selectors works

    Code
      print_lazy(elem_ancestors(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The ancestors.

---

    Code
      print_lazy(elem_ancestors(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first ancestor.

---

    Code
      print_lazy(elem_ancestors(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All ancestors except the first.

---

    Code
      print_lazy(elem_parent(element))
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The parent of this element.

---

    Code
      print_lazy(elem_siblings(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The siblings.

---

    Code
      print_lazy(elem_siblings(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first sibling.

---

    Code
      print_lazy(elem_siblings(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All siblings except the first.

---

    Code
      print_lazy(elem_children(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The direct children.

---

    Code
      print_lazy(elem_children(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first direct child.

---

    Code
      print_lazy(elem_children(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All direct children except the first.

---

    Code
      print_lazy(elem_descendants(element))
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * The descendants.

---

    Code
      print_lazy(elem_descendants(element)[[1]])
    Output
      A selenider element selecting:
      * The first element with css selector ".class".
      * The first descendant.

---

    Code
      print_lazy(elem_descendants(element)[-1])
    Output
      A collection of selenider elements selecting:
      * The first element with css selector ".class".
      * All descendants except the first.

# Lazy printing flattened objects works

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      A combination of elements.

---

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      The first of a combination of elements.

---

    Code
      print_lazy(elements[1:5])
    Output
      A collection of selenider elements selecting:
      The 1st, 2nd, 3rd, 4th, and 5th of a combination of elements.

---

    Code
      print_lazy(elements[-3])
    Output
      A collection of selenider elements selecting:
      All of a combination of elements except the third.

---

    Code
      print_lazy(elem_filter(elements, is_present))
    Output
      A collection of selenider elements selecting:
      The elements in a combination of elements that match the following
      condition:`is_present`

---

    Code
      print_lazy(elem_find(elements, is_present))
    Output
      A selenider element selecting:
      The first of a combination of elements matching the following
      condition:`is_present`

---

    Code
      print_lazy(elem_filter(elements, is_present, is_enabled))
    Output
      A collection of selenider elements selecting:
      The elements in a combination of elements that match a custom condition.

# Lazy printing results of elem_flatmap() works

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".

---

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".
      * The first element.

---

    Code
      print_lazy(elements[-1])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".
      * All elements except the first.

---

    Code
      print_lazy(elem_filter(elements, is_visible))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first child of each element with css selector "p".
      * The elements matching the following condition:
        `is_visible`

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The direct children of each element.

---

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first direct child of any element.

---

    Code
      print_lazy(elements[-seq_len(10)])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * All direct children of any element except the 1st, 2nd, 3rd, 4th, 5th, 6th,
        7th, 8th, 9th, and 10th.

---

    Code
      print_lazy(elem_find(elements, is_visible))
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first direct child of any element matching the following condition:
        `is_visible`

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.

---

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.
      * The first element.

---

    Code
      print_lazy(elements[-1])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.
      * All elements except the first.

---

    Code
      print_lazy(elem_filter(elements, is_visible))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The first direct child of each element.
      * The elements matching the following condition:
        `is_visible`

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.

---

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.
      * The first element.

---

    Code
      print_lazy(elements[-1])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.
      * All elements except the first.

---

    Code
      print_lazy(elem_filter(elements, is_visible))
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The children of each element with css selector "#id".
      * The parent of each element.
      * The elements matching the following condition:
        `is_visible`

---

    Code
      print_lazy(elements)
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The direct children of each element.
      * A transformation of each element using `elem_flatmap()`.

---

    Code
      print_lazy(elements[[1]])
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The direct children of any element.
      * A transformation of each element using `elem_flatmap()`.
      * The first element.

---

    Code
      print_lazy(elements[-seq_len(10)])
    Output
      A collection of selenider elements selecting:
      * The elements with css selector ".class".
      * The direct children of any element.
      * A transformation of each element using `elem_flatmap()`.
      * All elements except the 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th, and
        10th.

---

    Code
      print_lazy(elem_find(elements, is_visible))
    Output
      A selenider element selecting:
      * The elements with css selector ".class".
      * The direct children of any element.
      * A transformation of each element using `elem_flatmap()`.
      * The first element matching the following condition:
        `is_visible`

# Lazy printing works for results of JS expressions.

    Code
      print_lazy(js_element)
    Output
      A selenider element selecting:
      The result of a JavaScript expression.

---

    Code
      print_lazy(find_element(js_element, "p"))
    Output
      A selenider element selecting:
      * The result of a JavaScript expression.
      * The first child element with css selector "p".

---

    Code
      print_lazy(js_elements)
    Output
      A collection of selenider elements selecting:
      The results of a JavaScript expression.

---

    Code
      print_lazy(js_elements[1:2])
    Output
      A collection of selenider elements selecting:
      The 1st and 2nd results of a JavaScript expression.

---

    Code
      print_lazy(js_elements[[1]])
    Output
      A selenider element selecting:
      The first result of a JavaScript expression.

