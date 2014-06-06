using NabfProject;
using NabfProject.Actions;
using NabfProject.AI;
using NabfProject.Events;
using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using XmasEngine;
using XmasEngineController;
using XmasEngineModel.Management;
using XmasEngineModel.Management.Actions;
using XmasEngineModel.Management.Events;
using XmasEngineView;

namespace NabfServerApplication
{
    class Program
    {
        private static int simId = -1;
        private static int roundId = -1;
        private static int agentoffset = 3;
        private static int start = 0;
        private static Dictionary<NabfAgent, int> consolepos = new Dictionary<NabfAgent, int>();
        private static bool verbose = false;


        static void Main(string[] args)
        {
            //Console.Clear();
            string[] input;

            if (args.Length == 0)
                input = new string[] { "127.0.0.1:1337" };
            else
                input = args;

            string[] ipp = input[0].Split(new Char[]{':'});
            string ip = ipp[0];
            int port = Convert.ToInt32(ipp[1]);

            if (args.Length == 2 && args[1] == "verbose")
                verbose = true;

            TcpListener listener = new TcpListener(IPAddress.Parse(ip),port);

            listener.Start();

            EmptyWorldBuilder builder = new EmptyWorldBuilder();
            
            NabfModelFactory factory = new NabfModelFactory();

            NabfModel model = (NabfModel)factory.ConstructModel(builder);

            model.EventManager.Register(new Trigger<ActionCompletedEvent<AddXmasObjectAction>>(AddedXmasObject));
            model.EventManager.Register(new Trigger<ActionFailedEvent>(evt =>
                {
                    //Console.SetCursorPosition(0, 1);
                    Console.WriteLine("Error occured with " + evt.FailedAction.GetType().Name + ": " + evt.Exception.Message);
                }));
            model.EventManager.Register(new Trigger<NewNoticeEvent>(evt =>
                 {
                     
                 }));

            XmasEngineManager engine = new XmasEngineManager(factory);

            AgentMaster agentmaster = new AgentMaster(listener);

            engine.StartEngine(model, new XmasView[0], new XmasController[] { agentmaster });

        }

        private static void ReceivedMessage(EntityXmasAction<NabfAgent> action)
        {
            if (verbose)
            {
                //Console.SetCursorPosition(15, consolepos[action.Source] * 2 + agentoffset);
                Console.WriteLine("Received: " + action);
            }
            
        }

        private static void SendMessage(NabfAgent agent, XmasEvent evt)
        {
            if (verbose)
            {
                //Console.SetCursorPosition(15, consolepos[agent] * 2 + agentoffset + 1);
                Console.WriteLine("Sent: " + evt);
            }
            
        }

        private static void AddedXmasObject(ActionCompletedEvent<AddXmasObjectAction> evten)
        {
            if (evten.Action.Object is NabfAgent)
            {
                var agent = (NabfAgent)evten.Action.Object;
                Console.WriteLine("Agent " + agent.Name + " connected");
                //if (verbose)
				//{
				//	Console.SetCursorPosition(0, start * 2 + agentoffset);
				//	Console.Write("Agent: " + agent.Name);
				//}
                
                consolepos.Add(agent, start);
                //agent.Register(new Trigger<ActionStartingEvent<AddKnowledgeAction>>(evt => ReceivedMessage(evt.Action)));
                agent.Register(new Trigger<ActionStartingEvent<ApplyNoticeAction>>(evt => { if (evt.Action.NoticeId != -1) ReceivedMessage(evt.Action); }));
                //agent.Register(new Trigger<ActionStartingEvent<ChangeNoticeAction>>(evt => ReceivedMessage(evt.Action)));
                agent.Register(new Trigger<ActionStartingEvent<CreateNoticeAction>>(evt => Console.WriteLine("Agent "+agent.Name+" posted Job:\n" + evt.Action.JobType)));
                //agent.Register(new Trigger<ActionStartingEvent<DeleteNoticeAction>>(evt => ReceivedMessage(evt.Action)));
                //agent.Register(new Trigger<ActionStartingEvent<NewRoundAction>>(evt => ReceivedMessage(evt.Action)));
                //agent.Register(new Trigger<ActionStartingEvent<SubscribeSimulationAction>>(evt => ReceivedMessage(evt.Action)));
                agent.Register(new Trigger<ActionStartingEvent<AgentCrashed>>(evt =>
                    {
                        var message = "Agent "+evt.Action.Source.Name+" disconnected: \n  "+evt.Action.Exception.Message + " \n Stack trace:\n"+evt.Action.Exception.StackTrace;
                        //Console.SetCursorPosition(15, consolepos[agent] * 2 + agentoffset);
                        Console.WriteLine(message);

                    }));
                
                //agent.Register(new Trigger<NewKnowledgeEvent>(evt => SendMessage(agent, evt)));
                //agent.Register(new Trigger<NoticeRemovedEvent>(evt => SendMessage(agent, evt)));
                //agent.Register(new Trigger<NoticeUpdatedEvent>(evt => SendMessage(agent, evt)));
                agent.Register(new Trigger<ReceivedJobEvent>(evt => SendMessage(agent, evt)));
                //agent.Register(new Trigger<SimulationSubscribedEvent>(evt => SendMessage(agent, evt)));

                agent.Register(new Trigger<ActionStartingEvent<NewRoundAction>>(evt =>
                {
                    bool updated = false;
                    if (simId != evt.Action.SimId)
                    {
                        updated = true;
                        simId = evt.Action.SimId;
                    }
                    if (roundId != evt.Action.RoundNumber)
                    {
                        updated = true;
                        roundId = evt.Action.RoundNumber;
                    }
                    if (updated)
                    {
                        //Console.SetCursorPosition(0, 0);
                        Console.WriteLine("Begin simulation " + simId + ", Round: " + roundId);
                    }


                }));
                start++;
            }
        }
    }
}
