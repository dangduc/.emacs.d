"""Consolidate the frozen source pair into the renamed sidebar library."""
from pathlib import Path
import json

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
forms = json.loads((HERE / 'forms.json').read_text())

def rename(text):
    return (text.replace('agent-shell-sidebar--ensure-parsed', 'agent-sidebar--ensure-header')
            .replace('agent-shell-sidebar--sidebar-buffer', 'agent-sidebar--sidebar-window')
            .replace('agent-shell-sidebar', 'agent-sidebar'))

core = [{**form, 'name': rename(form['name']), 'text': rename(form['text'])}
        for form in forms['agent-shell-sidebar']]
front = [{**form, 'text': rename(form['text'])} for form in forms['agent-sidebar']
         if form['kind'] not in ('defalias', 'defvaralias')]
by_core = {form['name']: form['text'] for form in core}
by_front = {form['name']: form['text'] for form in front}

header = ''';;; agent-sidebar.el --- Browse agent-shell, Claude, and Codex chats -*- lexical-binding: t; coding: utf-8; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Keywords: agent-shell, claude, codex, tools
;; Package-Requires: ((emacs "29.1") (agent-shell "0.74.3") (project "0.9"))

;;; Commentary:
;; Groups file-backed conversations by provider, repository, model, and date.
;; This file owns discovery, metadata caching, idle parsing, and sidebar windows.
;; Entry point: M-x agent-sidebar-toggle-sidebar.
;; RET resumes; o reads; / filters; G changes grouping; N starts a new session.
;; Codex CLI sessions use ghostel; A resumes the selected Codex chat in agent-shell.

;;; Code:
'''

result = [header]
seen = set()
def emit(text):
    if text not in seen:
        result.append(text)
        seen.add(text)

for form in core + front:
    if form['kind'] in ('require', 'declare-function') or form['name'] in ('vterm-shell', 'projectile-known-projects'):
        if form['text'] != "(require 'agent-sidebar)":
            emit(form['text'])

result.append(';;; Customization and faces')
for form in front + core:
    if form['kind'] in ('defgroup', 'defcustom', 'defface'):
        if form['name'] not in seen:
            emit(form['text'])
            seen.add(form['name'])

result.append(';;; State')
for form in core + front:
    if form['kind'] in ('defvar', 'defvar-local', 'defconst') and form['name'] != 'agent-sidebar-mode-map':
        emit(form['text'])

# The former parent mode's window functions are now the only implementations.
core_preferred = {'agent-sidebar--get-or-create-buffer', 'agent-sidebar-show-sidebar',
                  'agent-sidebar-jump-to-sidebar'}
# Provider rendering replaces the old project-only renderer and group command.
obsolete = {'agent-sidebar--render-project-header', 'agent-sidebar--render-agent-header',
            'agent-sidebar--group-by-agent', 'agent-sidebar--render-transcript-row',
            'agent-sidebar-toggle-project', 'agent-sidebar--short-date'}
front_names = {form['name'] for form in front}
result.append(';;; Discovery, metadata, timers, and windows')
for form in core:
    if form['kind'] not in ('defun', 'iter-defun') or form['name'] in obsolete:
        continue
    if form['name'] in front_names and form['name'] not in core_preferred:
        continue
    text = form['text']
    if form['name'] == 'agent-sidebar--redraw':
        start = text.index('      (if agent-sidebar--render-contents-function')
        end = text.index('      (when (= (point-min)', start)
        text = text[:start] + '      (agent-sidebar--render-contents)\n' + text[end:]
        text = text.replace('           (inhibit-read-only t)\n           (groups agent-sidebar--groups))',
                            '           (inhibit-read-only t))')
    emit(text)

result.append(';;; Providers, grouping, and commands')
for form in front:
    if form['kind'] in ('defun', 'iter-defun', 'agent-sidebar-register-provider') and form['name'] not in core_preferred:
        emit(form['text'])

result.append(';;; Mode and keymap')
keymap = by_core['agent-sidebar-mode-map'].replace('agent-sidebar-toggle-project', 'agent-sidebar-toggle-group')
keymap = keymap.replace('    map)', '''    (define-key map (kbd "G") #'agent-sidebar-set-grouping)
    (define-key map (kbd "A") #'agent-sidebar-visit-in-agent-shell)
    map)''')
emit(keymap)
mode = by_core['agent-sidebar-mode']
body = mode[mode.index('  (setq-local truncate-lines'):]
body = body.removesuffix('  (agent-sidebar--schedule-refresh))')
front_body = by_front['agent-sidebar-mode']
front_body = front_body[front_body.index('  (setq-local agent-sidebar--entries'):].removesuffix(')')
emit('''(define-derived-mode agent-sidebar-mode special-mode "AgentChats"
  "Browse conversations across providers.
RET resumes, o reads, / filters, G changes grouping, and N starts a session.
A resumes a Codex rollout through agent-shell; C-u N starts a Codex agent-shell.
The d key marks a transcript, u removes its mark, and x confirms deletion.
\\\\{agent-sidebar-mode-map}"
  :group 'agent-sidebar
''' + body + front_body + '\n  (agent-sidebar--schedule-refresh))')

evil = next(form['text'] for form in core if form['kind'] == 'with-eval-after-load')
evil = evil.replace('agent-sidebar-toggle-project', 'agent-sidebar-toggle-group')
evil = evil.replace('(kbd "G")   #\'evil-goto-line', '(kbd "G")   #\'agent-sidebar-set-grouping')
evil = evil.replace('(kbd "N")   #\'agent-sidebar-new-session',
                    '(kbd "N")   #\'agent-sidebar-new-session\n      (kbd "A")   #\'agent-sidebar-visit-in-agent-shell')
emit(evil)
emit("(provide 'agent-sidebar)\n;;; agent-sidebar.el ends here")

output = '\n\n'.join(result) + '\n'
assert 'agent-shell-sidebar' not in output
assert 'defvaralias' not in output and '(defalias' not in output
(ROOT / 'lisp/agent-sidebar.el').write_text(output)
