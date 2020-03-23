LIBRARY IEEE;
     USE IEEE.STD_LOGIC_1164.all;
     USE IEEE.NUMERIC_STD.all;

     ENTITY contador_matrix IS
       GENERIC (width:POSITIVE:=4);
       PORT (clk   : IN std_logic; 
             reset : IN std_logic; 
             S_clk      : out std_logic;
             dout       : out std_logic;
             bcd_in_dec : std_logic_vector(3 downto 0);
             SS         : out std_logic
       );
     END contador_matrix;

     ARCHITECTURE arch1 OF contador_matrix IS
       SIGNAL cnt : UNSIGNED(width-1 DOWNTO 0):= (others => '0');
       SIGNAL clkbuffer : std_logic;
       signal counter : integer range 0 to 125000000;
       signal count   : std_logic_vector(width-1 downto 0);
       component test_mode is
         port (
            clk        : in std_logic;
            rst        : in std_logic;
            S_clk      : out std_logic;
            dout       : out std_logic;
            SS         : out std_logic;
--            data_en    : in std_logic;
            sw_in      : in std_logic_vector(1 downto 0);
            bcd_in_dec : in STD_LOGIC_VECTOR(3 downto 0);
            bcd_in_uni : in STD_LOGIC_VECTOR(3 downto 0)
         );
       end component;
     BEGIN
       
--       divisor: process(clk)
--       begin
--        if(rising_edge(clk)) then
--            counter <= counter + 1;
--            if(counter = 125000000) then
--            clkbuffer <= not clkbuffer;
--            counter <= 0;
--            end if;
--        end if;
--       end process;
       
--       contador : PROCESS (clkbuffer, reset) IS
--       BEGIN
--         IF reset = '1' THEN
--           cnt <= (others => '0');
--         ELSIF rising_edge(clkbuffer) THEN
--             cnt <= cnt + 1;
--         END IF;
--       END PROCESS;

--       count <= std_logic_vector(cnt);
       
       controlador_matriz: test_mode
        port map (
            clk => clk, 
            rst => reset,       
            S_clk => S_clk,     
            dout => dout,      
            SS => SS,        
            --data_en => '0',   
            sw_in => "00",     
            bcd_in_dec => bcd_in_dec,
            bcd_in_uni => "0000"
        );
     END arch1;