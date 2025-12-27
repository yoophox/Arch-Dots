(defun px/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (and (buffer-file-name buffer) (not (eq buffer (current-buffer))))
            (kill-buffer buffer)))
        (buffer-list)))
(global-set-key (kbd "C-!") #'px/kill-other-buffers)
