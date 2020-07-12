#!/usr/bin/env bash

# bash version

BLOCKLISTS=(
# Pedophiles
"http://list.iblocklist.com/?list=dufcxgnbjsdwmwctgfuj&fileformat=p2p&archiveformat=gz"
# level1
"http://list.iblocklist.com/?list=ydxerpxkpcfqjaybcssw&fileformat=p2p&archiveformat=gz"
# level2
"http://list.iblocklist.com/?list=gyisgnzbhppbvsphucsw&fileformat=p2p&archiveformat=gz"
# level3
"http://list.iblocklist.com/?list=uwnukjqktoggdknzrhgh&fileformat=p2p&archiveformat=gz"
# edu
"http://list.iblocklist.com/?list=imlmncgrkbnacgcwfjvh&fileformat=p2p&archiveformat=gz"
# rangetest
"http://list.iblocklist.com/?list=plkehquoahljmyxjixpu&fileformat=p2p&archiveformat=gz"
# bogon
"http://list.iblocklist.com/?list=gihxqmhyunbxhbmgqrla&fileformat=p2p&archiveformat=gz"
# ads
"http://list.iblocklist.com/?list=dgxtneitpuvgqqcpfulq&fileformat=p2p&archiveformat=gz"
# spyware
"http://list.iblocklist.com/?list=llvtlsjyoyiczbkjsxpf&fileformat=p2p&archiveformat=gz"
# proxy
"http://list.iblocklist.com/?list=xoebmbyexwuiogmbyprb&fileformat=p2p&archiveformat=gz"
# badpeers
"http://list.iblocklist.com/?list=cwworuawihqvocglcoss&fileformat=p2p&archiveformat=gz"
# Microsoft
"http://list.iblocklist.com/?list=xshktygkujudfnjfioro&fileformat=p2p&archiveformat=gz"
# spider
"http://list.iblocklist.com/?list=mcvxsnihddgutbjfbghy&fileformat=p2p&archiveformat=gz"
# hijacked
"http://list.iblocklist.com/?list=usrcshglbiilevmyfhse&fileformat=p2p&archiveformat=gz"
# dshield
"http://list.iblocklist.com/?list=xpbqleszmajjesnzddhv&fileformat=p2p&archiveformat=gz"
# forumspam
"http://list.iblocklist.com/?list=ficutxiwawokxlcyoeye&fileformat=p2p&archiveformat=gz"
# webexploit
"http://list.iblocklist.com/?list=ghlzqtqxnzctvvajwwag&fileformat=p2p&archiveformat=gz"
# iana-reserved
"http://list.iblocklist.com/?list=bcoepfyewziejvcqyhqo&fileformat=p2p&archiveformat=gz"
# iana-private
"http://list.iblocklist.com/?list=cslpybexmxyuacbyuvib&fileformat=p2p&archiveformat=gz"
# iana-multicast
"http://list.iblocklist.com/?list=pwqnlynprfgtjbgqoizj&fileformat=p2p&archiveformat=gz"
# fornonlancomputers
"http://list.iblocklist.com/?list=jhaoawihmfxgnvmaqffp&fileformat=p2p&archiveformat=gz"
# exclusions
"http://list.iblocklist.com/?list=mtxmiireqmjzazcsoiem&fileformat=p2p&archiveformat=gz"
# DROP
"http://list.iblocklist.com/?list=zbdlwrqkabxbcppvrnos&fileformat=p2p&archiveformat=gz"
# CINS Army
"http://list.iblocklist.com/?list=npkuuhuxcsllnhoamkvm&fileformat=p2p&archiveformat=gz"
# malc0de
"http://list.iblocklist.com/?list=pbqcylkejciyhmwttify&fileformat=p2p&archiveformat=gz"
# adservers
"http://list.iblocklist.com/?list=zhogegszwduurnvsyhdf&fileformat=p2p&archiveformat=gz"
# bogon
"http://list.iblocklist.com/?list=lujdnbasfaaixitgmxpp&fileformat=p2p&archiveformat=gz"
# cruzit web attacks
"http://list.iblocklist.com/?list=czvaehmjpsnwwttrdoyl&fileformat=p2p&archiveformat=gz"
)
TRANSMISSION_BLOCKLISTS_DIR=~/.config/transmission/blocklists

find "$TRANSMISSION_BLOCKLISTS_DIR" -name 'generated*' -delete

for list in "${BLOCKLISTS[@]}"; do
	wget "$list" -O - | gunzip >> "$TRANSMISSION_BLOCKLISTS_DIR/generated.txt"
done

gzip "$TRANSMISSION_BLOCKLISTS_DIR/generated.txt"

test -d ~/.config/transmission-daemon && cp "$TRANSMISSION_BLOCKLISTS_DIR/generated.txt.gz" ~/.config/transmission-daemon/blocklists/generated.txt.gz
