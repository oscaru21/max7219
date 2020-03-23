library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_mode is
 generic(
    numMatrix : integer := 3);
 port (
	clk        : in std_logic;
	rst        : in std_logic;
	S_clk      : out std_logic;
	dout       : out std_logic;
	SS         : out std_logic;
	sw_in      : in std_logic_vector(1 downto 0);
	bcd_in_dec : in STD_LOGIC_VECTOR(3 downto 0);
	bcd_in_uni : in STD_LOGIC_VECTOR(3 downto 0)
 );
end entity test_mode;

architecture RTL of test_mode is
--Max7219 Register Address Table 
constant MAX7219_REG_NO_OP_ADDRESS                         : std_logic_vector(7 downto 0) :=          x"00";
constant MAX7219_REG_ROW1_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"01";
constant MAX7219_REG_ROW2_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"02";
constant MAX7219_REG_ROW3_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"03";
constant MAX7219_REG_ROW4_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"04";
constant MAX7219_REG_ROW5_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"05";
constant MAX7219_REG_ROW6_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"06";
constant MAX7219_REG_ROW7_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"07";
constant MAX7219_REG_ROW8_ADDRESS                          : std_logic_vector(7 downto 0) :=          x"08";
constant MAX7219_REG_INTENSITY_ADDRESS                     : std_logic_vector(7 downto 0) :=          x"0A";
constant MAX7219_REG_SCAN_LIMIT_ADDRESS                    : std_logic_vector(7 downto 0) :=          x"0B";
constant MAX7219_REG_SHUTDOWN_ADDRESS                      : std_logic_vector(7 downto 0) :=          x"0C";
--Content of Max7219 Intensity Register 
constant MAX7219_REG_INTENSITY_MIN_VALUE                   : std_logic_vector(7 downto 0) :=          x"00";
constant MAX7219_REG_INTENSITY_MAX_VALUE                   : std_logic_vector(7 downto 0) :=          x"0F";
--Content of Max7219 Scan Limit Register 
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0            : std_logic_vector(7 downto 0) :=          x"00";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_1       : std_logic_vector(7 downto 0) :=          x"01";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_2       : std_logic_vector(7 downto 0) :=          x"02";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_3       : std_logic_vector(7 downto 0) :=          x"03";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_4       : std_logic_vector(7 downto 0) :=          x"04";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_5       : std_logic_vector(7 downto 0) :=          x"05";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_6       : std_logic_vector(7 downto 0) :=          x"06";
constant MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_7       : std_logic_vector(7 downto 0) :=          x"07";
--Content of Max7219 Shutdown Register
constant MAX7219_REG_SHUTDOWN_MODE_SHUTDOWN_MODE           : std_logic_vector(7 downto 0) :=          x"00";
constant MAX7219_REG_SHUTDOWN_MODE_NORMAL_OPERATION        : std_logic_vector(7 downto 0) :=          x"01";
--Content of Max7219 Display Test Register
constant MAX7219_REG_DISPLAY_TEST_NORMAL_OPERATION         : std_logic_vector(7 downto 0) :=          x"00";
constant MAX7219_REG_DISPLAY_TEST_DISPLAY_TEST_MODE        : std_logic_vector(7 downto 0) :=          x"01";

constant MAX7219_CLEAR_ROW                                 : std_logic_vector(7 downto 0) :=          x"00";
--ARRAY MATRICES
  type matrix is array (1 to 8*numMatrix) of std_logic_vector(7 downto 0);
  signal mensaje : matrix := (others => "00000000");
  signal mensajebuffer : matrix := (others => "00000000");
--maquina de estados
  type state_type is (idle, ena, disa);
  signal state      : state_type := idle;
  
--declaracion de se�ales protocolo spi
  signal enable     : std_logic;
  signal r_busy     : std_logic;
  signal writeval   : std_logic_vector ((16*numMatrix)-1 downto 0);
  signal data       : std_logic_vector ((16*numMatrix)-1 downto 0);
  signal ss_n       : STD_LOGIC_VECTOR(3 DOWNTO 0);
  signal rx_data    : STD_LOGIC_VECTOR((16*numMatrix)-1 DOWNTO 0);
  signal miso       : std_logic;
  signal SPIclk     : std_logic;
  signal r_rst      : std_logic := '1';  
  --banderas
  signal fsmdone       : std_logic;	 
  signal matrixready   : boolean := false;
  signal setupCounter  : integer range 0 to 13 := 0;
  signal d_enable      : std_logic := '0';
  signal simbolo       : std_logic_vector(63 downto 0);
  signal unidades      : std_logic_vector(63 downto 0);
  signal decenas       : std_logic_vector(63 downto 0);
  signal counter       : integer range 0 to 50000000 := 0;
  signal clockbuffer     : std_logic;
component spi_master IS
  GENERIC(
    slaves  : INTEGER := 4;  --number of spi slaves
    d_width : INTEGER := 16*numMatrix); --data bus width
  PORT(
    clock   : IN     STD_LOGIC;                             --system clock
    reset_n : IN     STD_LOGIC;                             --asynchronous reset
    enable  : IN     STD_LOGIC;                             --initiate transaction
    cpol    : IN     STD_LOGIC;                             --spi clock polarity
    cpha    : IN     STD_LOGIC;                             --spi clock phase
    cont    : IN     STD_LOGIC;                             --continuous mode command
    clk_div : IN     INTEGER;                               --system clock cycles per 1/2 period of sclk
    addr    : IN     INTEGER;                               --address of slave
    tx_data : IN     STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data to transmit
    miso    : IN     STD_LOGIC;                             --master in, slave out
    sclk    : BUFFER    STD_LOGIC;                             --spi clock
    ss_n    : BUFFER    STD_LOGIC_VECTOR(slaves-1 DOWNTO 0);   --slave select
    mosi    : OUT    STD_LOGIC;                             --master out, slave in
    busy    : OUT    STD_LOGIC;                             --busy / data ready signal
    rx_data : OUT    STD_LOGIC_VECTOR(d_width-1 DOWNTO 0)); --data received
end component;

component bcd_to_LEDvector is
    Port ( bcd_in : in STD_LOGIC_VECTOR (3 downto 0);
           LEDvector : out STD_LOGIC_VECTOR (63 downto 0));
end component;

component sw_to_LEDvector is
    Port ( sw_in : in STD_LOGIC_VECTOR (1 downto 0);
           LEDvector : out STD_LOGIC_VECTOR (63 downto 0));
end component;

procedure setupMatrix ( address, data: IN std_logic_vector(7 downto 0);
                       signal output: OUT std_logic_vector((16*numMatrix)-1 downto 0)) is
begin
    for i in 1 to numMatrix loop
        output(((16*i)-1) downto ((16*i)-16)) <= (address & data);
    end loop;
end setupMatrix;

procedure writeMatrix ( address : IN std_logic_vector(7 downto 0);
                        signal matriz  : IN matrix;
                        signal output: OUT std_logic_vector((16*numMatrix)-1 downto 0)) is
begin
    for i in 1 to numMatrix loop
        output(((16*i)-1) downto ((16*i)-16)) <= (address & matriz((8*i)-(to_integer(unsigned(address))-1)));
    end loop;
end writeMatrix;

procedure fillMessage ( signal vector : IN std_logic_vector(63 downto 0);
                       signal matriz : out matrix; 
                       n : integer range 0 to numMatrix-1) is
begin
    for i in 1 to 8 loop
         matriz(i+(8*n)) <= vector(((8*i)-1) downto ((8*i)-8));
    end loop;
end fillMessage;
begin
fillMessage(decenas,  mensaje, 0);
fillMessage(unidades, mensaje, 1);
fillMessage(simbolo,  mensaje, 2);

r_rst <= not rst;
S_clk <= SPIclk;
SS <= ss_n(0);

-- proceso que habilita y desabilita la nueva entrada de datos
fsm : process (clk, r_rst)
begin
if r_rst = '0' then
   state <= idle;
   fsmdone<= '0';
   
elsif ( rising_edge(clk) ) then
	case state is
		when idle =>
					
		if (r_busy = '0'and (not matrixready or d_enable = '0')) then
		    fsmdone <= '0';
			state <= ena;
		else
			state <= idle;
		end if;					
		when ena =>			
			enable <= '1';
			data   <= writeval;
			state  <= disa;		
		when disa =>	
			enable <= '0';
			fsmdone <= '1';
			state <= idle;
	end case;
end if;
end process fsm;

clock1hz: Process(clk)
	begin
		if( rising_edge(clk) ) then
			counter <= counter + 1;
			if( counter = 5000 ) then			
				clockbuffer <= not clockbuffer;
				mensajebuffer <= mensaje;
				counter <= 0;
			end if;	
		end if;
	end process;

configuracion: process( fsmdone ) 
begin
	if( rising_edge(fsmdone) ) then			
		if( not matrixready ) then
			case setupCounter is
				when 0 => setupMatrix(MAX7219_REG_NO_OP_ADDRESS, MAX7219_REG_NO_OP_ADDRESS, writeval);
				when 1 => setupMatrix(MAX7219_REG_SHUTDOWN_ADDRESS, MAX7219_REG_SHUTDOWN_MODE_NORMAL_OPERATION, writeval);
				when 2 => setupMatrix(MAX7219_REG_SCAN_LIMIT_ADDRESS, MAX7219_REG_SCAN_LIMIT_DISPLAY_DIGIT_0_TO_7, writeval);
				when 3 => setupMatrix(MAX7219_REG_INTENSITY_ADDRESS, MAX7219_REG_INTENSITY_MAX_VALUE, writeval);
				when 4 => setupMatrix(MAX7219_REG_ROW1_ADDRESS, MAX7219_CLEAR_ROW, writeval);				
				when 5 => setupMatrix(MAX7219_REG_ROW2_ADDRESS, MAX7219_CLEAR_ROW, writeval);				
				when 6 => setupMatrix(MAX7219_REG_ROW3_ADDRESS, MAX7219_CLEAR_ROW, writeval);
				when 7 => setupMatrix(MAX7219_REG_ROW4_ADDRESS, MAX7219_CLEAR_ROW, writeval);				
				when 8 => setupMatrix(MAX7219_REG_ROW5_ADDRESS, MAX7219_CLEAR_ROW, writeval);				
				when 9 => setupMatrix(MAX7219_REG_ROW6_ADDRESS, MAX7219_CLEAR_ROW, writeval);
				when 10 => setupMatrix(MAX7219_REG_ROW7_ADDRESS, MAX7219_CLEAR_ROW, writeval);
				when 11 => setupMatrix(MAX7219_REG_ROW8_ADDRESS, MAX7219_CLEAR_ROW, writeval);
				when 12 => matrixready <= true; setupCounter <= 0 ; writeval <= (others => '0');
				--when 12 => writeMatrix(MAX7219_REG_ROW5_ADDRESS, mensajebuffer, writeval);
				when others => writeval <= (others => '0');
			end case;
			setupCounter <= setupCounter + 1;
        elsif(matrixready) then
		  case setupCounter is
				when 0 => writeMatrix(MAX7219_REG_ROW1_ADDRESS, mensaje, writeval);
				when 1 => writeMatrix(MAX7219_REG_ROW2_ADDRESS, mensaje, writeval);
				when 2 => writeMatrix(MAX7219_REG_ROW3_ADDRESS, mensaje, writeval);
				when 3 => writeMatrix(MAX7219_REG_ROW4_ADDRESS, mensaje, writeval);
				when 4 => writeMatrix(MAX7219_REG_ROW5_ADDRESS, mensaje, writeval);
				when 5 => writeMatrix(MAX7219_REG_ROW6_ADDRESS, mensaje, writeval);
				when 6 => writeMatrix(MAX7219_REG_ROW7_ADDRESS, mensaje, writeval);
				when 7 => writeMatrix(MAX7219_REG_ROW8_ADDRESS, mensaje, writeval);
				when 8 => setupCounter <= 0; 
				when others => writeval <= (others => '0');				
			end case;
			setupCounter <= setupCounter + 1;
		end if;
	end if;					
end process configuracion;

SPI: spi_master port map
(
 reset_n => r_rst,
 clock   => clk,
 tx_data  => data,
 enable    => enable,
 cpol => '0',
 cpha => '0',
 cont => '0',
 clk_div => 10,
 addr => 0,
 miso => miso,
 busy => r_busy,
 sclk  => SPIclk,
 ss_n => ss_n,
 mosi => dout,
 rx_data => rx_data  
);	
leddecenas: bcd_to_LEDvector
port map (
bcd_in => bcd_in_dec,
LEDvector => decenas
);
ledunidades: bcd_to_LEDvector
port map (
bcd_in =>  bcd_in_uni,
LEDvector => unidades
);
ledsimbolo: sw_to_LEDvector
port map (
sw_in =>  sw_in,
LEDvector => simbolo
);
 
end architecture RTL;