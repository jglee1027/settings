" ---- language-env DON'T MODIFY THIS LINE!
""" ========================================================
""" 기본적인 설정들
""" ========================================================
set nocompatible    " Vim 디폴트 기능들을 사용함
set backspace=2     " 삽입 모드에서 백스페이스를 계속 허용
set autoindent      " 자동 들여쓰기
set cindent         " C 언어 자동 들여쓰기
set smartindent     " 역시 자동 들여쓰기
set textwidth=76    " 76번째 칸을 넘어가면 자동으로 줄 바꿈
set nowrapscan      " 찾기에서 파일의 맨 끝에 이르면 계속하여 찾지 않음
"set nobackup       " 백업파일을 만들지 않음
set novisualbell    " 비주얼벨 기능을 사용하지 않음
set nojoinspaces    " J 명령어로 줄을 붙일 때 마침표 뒤에 한칸만 띔
set ruler           " 상태표시줄에 커서 위치를 보여줌
set tabstop=4       " <Tab> 간격
set shiftwidth=4    " 자동 들여쓰기 간격
"set keywordprg=edic    " K를 눌렀을 때 실행할 명령어
set showcmd         " (부분적인) 명령어를 상태라인에 보여줌
set showmatch       " 매치되는 괄호의 반대쪽을 보여줌
set ignorecase      " 찾기에서 대/소문자를 구별하지 않음
set incsearch       " 점진적으로 찾기
set hlsearch		" 찾는 단어 하이라이팅
set autowrite       " :next 나 :make 같은 명령를 입력하면 자동으로 저장
set title			" 타이틀바에 현재 편집중인 파일을 표시

set selectmode=mouse,key
 
""" ========================================================
""" 파일 인코딩을 한국어로 설정
""" ========================================================
if $LANG[0] == 'k' && $LANG[1] == 'o'
  set fileencoding=korea
endif


""" ========================================================
""" 터미널에 따른 설정 : Xterm이면 16컬러 사용
""" ========================================================
if &term =~ "xterm-debian" || &term =~ "xterm-xfree86"
  set t_Co=16
  set t_Sf=^[[3%dm
  set t_Sb=^[[4%dm
  set t_kb=
  fixdel
endif


""" ========================================================
""" 문법 강조기능 사용
""" ========================================================
if has("syntax")
  syntax on
endif


""" ========================================================
""" GUI 모드로 실행할 경우
""" ========================================================
if has("gui_running")
  set visualbell	" 비주얼벨 기능 사용
  set hlsearch		" 찾는 단어를 하이라이팅
  set guifontset=-*-fixed-medium-r-normal--14-*-75-75-*-70-iso8859-1,-*-gulim-medium-r-normal--14-140-75-75-*-140-ksc5601.1987-0
endif

""" ========================================================
""" cscope
""" ========================================================
set csprg=/usr/bin/cscope "cscope
set csto=0
set cst
set nocsverb

if filereadable("./cscope.out")
	cs add ./cscope.out
else
	cs add /usr/src/linux/cscope.out
endif

set csverb