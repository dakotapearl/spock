package interfaceDomain.swing;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import tools.errorChecking.Log;

import experimentDomain.Experiment;
import experimentDomain.Binary.BinaryAND;

import interfaceDomain.Interface;
import interfaceDomain.InterfaceDomain;

public class SwingInterface extends Interface {

	JFrame frame;
	JPanel mainPanel, startStopPanel;
	Experiment exp;
	
	private class startButtonListener implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent arg0) {
			Log.write("Interface (1): selecting experiment");
			exp = new BinaryAND(interfaceDomain.experimentDomain);
			
			Log.write("Interface (2): setting parameters");
			
			// Concurrent, no ordering after this start call can be guaranteed 
			Log.write("Interface (3): start threads");
			exp.start();
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
		
		JLabel label = new JLabel("Start?");
		mainPanel.add(label);
        
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

}
