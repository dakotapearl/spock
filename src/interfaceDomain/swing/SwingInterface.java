package interfaceDomain.swing;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Observable;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import tools.errorChecking.Assert;
import tools.errorChecking.Log;

import experimentDomain.Experiment;
import experimentDomain.Binary.BinaryAND;

import interfaceDomain.Interface;
import interfaceDomain.InterfaceDomain;
import interfaceDomain.InterfaceObservable;

public class SwingInterface extends Interface {

	JFrame frame;
	JPanel mainPanel, startStopPanel;
	JLabel statusLabel;
	JPanel expPanel, envPanel, netPanel;
	JLabel expTitle, envTitle, netTitle;
	JLabel expname;
	JLabel envname, envtype;
	JLabel nodes,activenodes,establishedpathways;
	
	Experiment exp;
	
	//TODO Might not synchronise properly with initialisation of componets, check 
	private void setObservables() {
		// Add all network interface variables
		for (String id : interfaceDomain.networkDomain.getNetwork().interfaceObservables.keySet())
			interfaceDomain.networkDomain.getNetwork().interfaceObservables.get(id).addObserver(this);
		
		// Add all network node interface variables
		/*for (String id : exp..interfaceObservables.keySet())
			interfaceDomain.networkDomain.getNetwork().interfaceObservables.get(id).addObserver(this);
		
		// Add all experiment interface variables
		for (String id : interfaceDomain.networkDomain.getNetwork().interfaceObservables.keySet())
			interfaceDomain.networkDomain.getNetwork().interfaceObservables.get(id).addObserver(this);
		
		// Add all environment interface variables
		for (String id : interfaceDomain.networkDomain.getNetwork().interfaceObservables.keySet())
			interfaceDomain.networkDomain.getNetwork().interfaceObservables.get(id).addObserver(this);*/
	}
	
	private class startButtonListener implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent arg0) {
			Log.write("Interface (1): selecting experiment");
			exp = new BinaryAND(interfaceDomain.experimentDomain);
			
			Log.write("Interface (2): setting parameters");
			
			// Concurrent, no ordering after this start call can be guaranteed 
			Log.write("Interface (3): start threads");
			statusLabel.setText("Status: Started");
			exp.start();
			setObservables();
		}
		
	}
	
	private class exitButtonListener implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent arg0) {
			// stop experiment
			// save network(experiment?) state
			// transfer back across network connect if necessary
			
			// Exit
			System.exit(1);
			
		}
		
	}
	
	public SwingInterface(InterfaceDomain interfaceDomain) {
		super(interfaceDomain);
	}
	
	@Override
	public void initialise() {
		frame = new JFrame("HelloWorldSwing");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		frame.getContentPane().add(mainPanel);
		
		startStopPanel = new JPanel();
		startStopPanel.setLayout(new GridLayout(1, 2));
		mainPanel.add(startStopPanel, BorderLayout.PAGE_END);
		
		
		expPanel = new JPanel();
		expPanel.setLayout(new GridLayout(5, 1));
		
		expTitle = new JLabel("Experiment     ");
		expPanel.add(expTitle);
		expname = new JLabel("Name: ");
		expPanel.add(expname);
		
		
		envPanel = new JPanel();
		envPanel.setLayout(new GridLayout(5, 1));
		
		envTitle = new JLabel("Environment    ");
		envPanel.add(envTitle);
		envname = new JLabel("Name: ");
		envPanel.add(envname);
		
		
		netPanel = new JPanel();
		netPanel.setLayout(new GridLayout(5, 1));
		
		netTitle = new JLabel("Network        ");
		netPanel.add(netTitle);
		nodes = new JLabel("Nodes: 0");
		netPanel.add(nodes);
		activenodes = new JLabel("Node activations: 0");
		netPanel.add(activenodes);
		establishedpathways = new JLabel("Established pathways: 0");
		netPanel.add(establishedpathways);
		
		mainPanel.add(expPanel, BorderLayout.LINE_START);
		mainPanel.add(netPanel, BorderLayout.CENTER);
		mainPanel.add(envPanel, BorderLayout.LINE_END);
		
		
		statusLabel = new JLabel("Status: Stopped");
		mainPanel.add(statusLabel, BorderLayout.PAGE_START);
        
        JButton startButton = new JButton("Start");
        startStopPanel.add(startButton);
        startButton.addActionListener(new startButtonListener());
		
        JButton exitButton = new JButton("Exit");
        startStopPanel.add(exitButton);
        exitButton.addActionListener(new exitButtonListener());
        
		frame.pack();
	}

	@Override
	public void run() {
		Log.writeForThreadCreation("Interface");
		
		frame.setVisible(true);
	}

	@Override
	public void update(Observable arg0, Object arg1) {
		if (arg0 instanceof InterfaceObservable) {
			if (((InterfaceObservable) arg0).getID() == "Number of nodes") {
				nodes.setText("Nodes: " + (String) arg1);
			} else if (((InterfaceObservable) arg0).getID() == "Node activations") {
				activenodes.setText("Node activations: " + (String) arg1);
			}
			
			
			
		} else {
			Assert.CriticalAssertTrue("Should not happen - observable not InterfaceObservable", false);
		}
	}

}
