using System;
using System.Collections.Generic;

namespace Crashing_Robots1
{
    class Robot
    {
        public int x, y, maxWidth, maxHeight,id;
        public string facing;
        public Robot(int id,int x, int y, int maxWidth, int maxHeight, string facing)
        {
            this.id = id;
            this.x = x;
            this.y = y;
            this.maxWidth = maxWidth + 1;
            this.maxHeight = maxHeight + 1;
            this.facing = facing;   
        }
        public bool Move()
        {
            switch (facing)
            {
                case "N":
                    y += 1;
                    if (y == maxHeight)
                        return false;
                    break;
                case "W":
                    x -= 1;
                    if (x == 0)
                        return false;
                    break;
                case "S":
                    y -= 1;
                    if (y == 0)
                        return false;
                    break;
                case "E":
                    x += 1;
                    if (x == maxWidth)
                        return false;
                    break;
                default:
                    return false;
            }
            return true;
        }
        public void Turn(string turn)
        {
            switch (facing)
            {
                case "N":
                    if (turn.Equals("L"))
                        facing = "W";
                    else facing = "E";
                    break;
                case "W":
                    if (turn.Equals("L"))
                        facing = "S";
                    else facing = "N";
                    break;
                case "S":
                    if (turn.Equals("L"))
                        facing = "E";
                    else facing = "W";
                    break;
                case "E":
                    if (turn.Equals("L"))
                        facing = "N";
                    else facing = "S";
                    break;
                default:
                    break;
            }
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            int cases;
            int width;
            int height;
            int numBots;
            int instructions;

            string msg;
            cases = Int32.Parse(Console.ReadLine());
            for (int a = 0; a < cases; a++)
            {

                msg = "OK";
                bool ok = true;
                string[] inp = Console.ReadLine().Split(" ");
                width = Int32.Parse(inp[0]);
                height = Int32.Parse(inp[1]);

                inp = Console.ReadLine().Split(" ");
                numBots = Int32.Parse(inp[0]);
                instructions = Int32.Parse(inp[1]);

                List<Robot> bots = new List<Robot>();
                for(int i = 0; i < numBots; i++)
                {
                    inp = Console.ReadLine().Split(" ");
                    bots.Add(new Robot( i+1,Int32.Parse(inp[0]), Int32.Parse(inp[1]), width, height, inp[2]));
                }
                for (int i = 0; i < instructions; i++)
                {
                    inp = Console.ReadLine().Split(" ");
                    if (!ok)
                        continue;
                    for (int j = 0; j < Int32.Parse(inp[2]); j++)
                    {
                        if (!ok)
                            break;
                        switch (inp[1])
                        {
                            case "F":
                                if (!bots[Int32.Parse(inp[0])-1].Move())
                                {
                                    Console.WriteLine();
                                    msg = "Robot "+ inp[0] + " crashes into the wall";
                                    ok = false;
                                    break;
                                }
                                if (CheckColision(bots[Int32.Parse(inp[0])-1], bots) != null)
                                {
                                    msg = "Robot "+ inp[0] + " crashes into robot " + (CheckColision(bots[Int32.Parse(inp[0])-1], bots)).id + "";
                                    ok = false;
                                }  
                                break;

                            default:
                                bots[Int32.Parse(inp[0]) - 1].Turn(inp[1]);
                                break;
                        }
                    }
                }
                Console.WriteLine(msg);
            }

        }
        static Robot CheckColision(Robot bot, List<Robot> bots)
        {
            foreach (Robot b in bots)
            {
                if (b.id == bot.id)
                    continue;
                if (b.x == bot.x && b.y == bot.y)
                    return b;
            }
            return null;
        }
    }
}