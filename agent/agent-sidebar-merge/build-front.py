from pathlib import Path
incoming=Path('agent/agent-sidebar-merge/incoming.el').read_text()
shared=['name','width','display-alist','pop-to-sidebar-on-toggle-open','no-delete-other-windows','resize-on-open','window-fixed','parse-chunk-size','parse-idle-delay','open-file-in-most-recently-used-window','refresh-timer','extra-project-roots','collapse-empty-projects','include-remote-projects']
state=['parse-cache','parse-timer','parse-queue','marks','collapsed','refresh-timer-object','file-info','pending-count','filter']
header=''';;; agent-sidebar.el --- Browse agent-shell and Claude CLI chats -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, codex, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.74.3") (project "0.9"))

;;; Commentary:
;; Groups file-backed conversations by provider, repository, model, and date.
;; Uses the companion agent-shell-sidebar.el for parsing queues, cache ownership,
;; staged rendering, window handling, and deletion checks.  Keep both files on
;; load-path.  RET resumes; o reads; / filters; G changes grouping; N starts anew.
;; Claude CLI sessions use ghostel, vterm, term, or a configured terminal runner.

;;; Code:
(require 'agent-shell-sidebar)
(require 'json)
(require 'term)

(declare-function ghostel-exec "ghostel")
(declare-function vterm "vterm")
(defvar vterm-shell)

'''
for name in shared+['--'+n for n in []]:
    header+=f"(defvaralias 'agent-sidebar-{name} 'agent-shell-sidebar-{name})\n"
for name in state:
    header+=f"(defvaralias 'agent-sidebar--{name} 'agent-shell-sidebar--{name})\n"
header+='\n'+incoming[incoming.index(';;; Customization'):incoming.index(';;; State')]
Path('lisp/agent-sidebar.el').write_text(header)
