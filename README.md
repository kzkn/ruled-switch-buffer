# Emacs Rule Based Switch Buffer Framework
Toggle paired buffers (ex; production code and test code) quickly based rules that written by yourself.

## Installation

WIP

## Usage

First, you must write rules. Here is some examples:

```elisp
;; rules for toggle production code and rspec code
(rule-based-switch-buffer-define app-rb-to-spec-rb
  :matcher (lambda (fn) (string-match "/app/.*.rb$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\(.*\\)/app/\\(.*\\).rb$" "\\1/spec/\\2_spec.rb" fn)))

(rule-based-switch-buffer-define spec-rb-to-app-rb
  :matcher (lambda (fn) (string-match "/spec/.*_spec.rb$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\(.*\\)/spec/\\(.*\\)_spec.rb$" "\\1/app/\\2.rb" fn)))

;; rules for viewcomponent gem
(rule-based-switch-buffer-define view-component-rb
  :matcher (lambda (fn) (string-match "_component.rb$" fn))
  :mappers ((lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.haml" fn))
            (lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.erb" fn))))

(rule-based-switch-buffer-define view-component-haml
  :matcher (lambda (fn) (string-match "_component.html.haml$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\.html.haml$" ".rb" fn)))

(rule-based-switch-buffer-define view-component-erb
  :matcher (lambda (fn) (string-match "_component.html.erb$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\.html.erb$" ".rb" fn)))

;; rules for c/c++
(rule-based-switch-buffer-define h-to-c
  :matcher (lambda (fn) (string-match ".h$" fn))
  :mappers ((lambda (fn) (replace-regexp-in-string "\\.h$" ".c" fn))
            (lambda (fn) (replace-regexp-in-string "\\.h$" ".cc" fn))
            (lambda (fn) (replace-regexp-in-string "\\.h$" ".cxx" fn))))

(rule-based-switch-buffer-define c-to-h
  :matcher (lambda (fn) (string-match ".c\\(c\\|xx\\)?$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\.c.*$" ".h" fn)))
```

Once the rules are ready, open any file and execute `M-x rule-based-switch-buffer`.
If the file name of current buffer matches some rules, a list of
possible destination files will be displayed and you will be asked to
select one. If there is only one candidate, it will be selected
automatically.

For example, if you have a file like the following:

```
foo.c
foo.cc
foo.h
```

Execute `M-x rule-based-switch-buffer` on foo.c or foo.cc will open foo.h immediately.
Execute `M-x rule-based-switch-buffer` on foo.h will ask you to choice whether to open foo.c or foo.cc.
