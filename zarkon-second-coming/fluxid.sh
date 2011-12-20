rm *.fasl
rm log.txt
rm proxy-bot.log
rm -rf images
mkdir images
cd ../fluxid
python tcpclient.py ants.fluxid.pl 2081 "../zarkon-second-coming/MyBot" zarkon_24_a blablabla

