clc;
clear all;

M = 63000;                    %���������� �������� ��� ������ �����-�����
P = 0;                        %��������
B = 0;                        %����� �����, ����������� �� ������� � ��� ��������
C = 0;                        %����� �������, ����� ����� ����� �� ������� � ��� �������� ���� �����������
NumberOfCars = 100;
NumberOfAllCars = 500;       %����� ����� ����� � ������
N = NumberOfCars*2;          %����� ������� ���� �������������� � ���� ����
Pnapr = 0;
Pmoshn = 0;
Perc = 0;                   %������� �������������� ����������� � V2G
PowerPotr = 0.05;          %������������ �������� ����� �������� ������� (���)
PowerVyd = 0.05;          %���������� �������� � V2G ����� �������������� (���)
R = 0;                     %���������� ��� ��������


Prob = xlsread('D:\nir\hackaton\MatabData.xlsx');         %������������ ������ My Electric Avenue
SoC = xlsread('D:\nir\hackaton\SoCforMatlab.xlsx');        %������������ ������ My Electric Avenue


Rastr=actxserver('Astra.Rastr');                          %������ MatLab � RastrWin3
Rastr.Load(1, 'D:\nir\hackaton\rezh1.rg2', '');            %����� ���� �� RastrWin3

Rastr.rgm('')

RastrTables=Rastr.Tables;
tnode=RastrTables.Item('node');
tvetv=RastrTables.Item('vetv');

tvras=tnode.Cols.Item('vras');
tdelta=tnode.Cols.Item('delta');
tnom=tnode.Cols.Item('ny');
ttip=tnode.Cols.Item('tip');
tbsh=tnode.Cols.Item('bsh');
tgsh=tnode.Cols.Item('gsh');
tuhom=tnode.Cols.Item('uhom');
tvzd=tnode.Cols.Item('vzd');
tpn=tnode.Cols.Item('pn');
tqn=tnode.Cols.Item('qn');
tqg=tnode.Cols.Item('qg');
tpg=tnode.Cols.Item('pg');

tr=tvetv.Cols.Item('r');
tx=tvetv.Cols.Item('x');
tip=tvetv.Cols.Item('ip');
tiq=tvetv.Cols.Item('iq');
tktr=tvetv.Cols.Item('ktr');
tsta=tvetv.Cols.Item('sta');
tib=tvetv.Cols.Item('ib');
tb=tvetv.Cols.Item('b');
ttipv=tvetv.Cols.Item('tip');
tkti=tvetv.Cols.Item('kti');
tvip=tvetv.Cols.Item('v_ip');

kvetv=tvetv.Size;
kusl=tnode.Size;

Unom=zeros(kusl, 1);
Pn=zeros(kusl, 1);
Qn=zeros(kusl, 1);

Pn2=zeros(kusl, M);
Qn2=zeros(kusl, M);
U=zeros(kusl, 1);
U2=zeros(kusl, 1);
U3=zeros(kusl, M);


for i=1:kusl
   Unom(i, 1)=tuhom.get('Z', i-1); 
   U(i, 1)=tvras.get('Z', i-1);
end



for i=1:kusl
   Pn(i, 1)=tpn.get('Z', i-1); 
   Qn(i, 1)=tqn.get('Z', i-1);
end


Pn1=Pn;
Qn1=Qn;
Pn3=Pn;
Qn3=Qn;

Y=zeros((kusl-1), M);
Z=zeros((kusl-1), M);

IfSoc=zeros(M, 1);


for i=1:numel(SoC)
if SoC(i, 1)>=7
    IfSoc(i, 1) = 1;
else IfSoc(i, 1) = 0;
end
end



for k=1:M

%�������� ��� ������� ����    

for i=1:(kusl-1)
    Y(i, k)=sum(datasample(Prob,N));             %���-�� ������������ ����� � ��� ��������
    Z(i, k)=sum(datasample(IfSoc,N))/N;           %������� ����� � ����������� ������� ������� ��� V2G
    Pn1((i+1), 1)=Pn((i+1), 1)+Y(i, k)*PowerPotr-Y(i, k)*Perc*Z(i, k)*PowerVyd; 
    Qn1((i+1), 1)=Qn((i+1), 1)+Y(i, k)*PowerPotr*(tan(acos(0.95)))-Y(i, k)*Perc*Z(i, k)*PowerVyd*(tan(acos(0.95)));
end


for i=1:kusl
  tpn.set('Z', i-1 , Pn1(i, 1) );
  tqn.set('Z', i-1 , Qn1(i, 1) );
end


for i=1:kusl
   Pn2(i, k)=tpn.get('Z', i-1);
   Qn2(i, k)=tqn.get('Z', i-1);
end





Rastr.rgm('')



for i=1:kusl
   U3(i, k)=tvras.get('Z', i-1);
end
 
for i=1:kusl
   Pn3(i, k)=tpn.get('Z', i-1);
end



if U3(kusl, k) < 9.5
       Pnapr = Pnapr+1;
end
 
PnMAX=max(Pn3);

if PnMAX > 3.2
       Pmoshn = Pmoshn+1;
end

   
disp(k);


end
 




PercOfCars = NumberOfCars*100/NumberOfAllCars      %������� �������������� �� ������ ����� �����������
YSum1 = sum(Y);
YSum = sum(YSum1);
Ysredn = ceil(YSum/(M*(kusl-1)));            %������� ����� ��������������, ������������ ������������ � ��� ��������
NumberOfStations = NumberOfCars/Ysredn      %��������� ���������� �������������� �� ���� �������� �������



PeregrNapr = Pnapr*100/M            %����������� �������� ���������� �� ��������� ���� ���� 9.5 ��
PeregrMoshn = Pmoshn*100/M         %����������� ���������� ������������




%������ � (����)
A = zeros(kusl, 1);
for i=1:kusl
A (i, 1) = i;
end

%�������

figure(10)

cmap=hsv(M); 



for i=1:M
plot(A(:,1),U3(:,i),'Color',cmap(i,:));
hold on;
end

axis([1, kusl, 8, 10.2])
grid on;
title('���������� � �����');
xlabel('����� ����');
ylabel('�������� ����������, ��');
hold off;





