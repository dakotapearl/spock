package interfaceDomain.swing;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.Observable;
import javax.swing.JFrame;
import javax.swing.JPanel;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;
import experimentDomain.Experiment;
import interfaceDomain.Interface;
import interfaceDomain.InterfaceDomain;
import interfaceDomain.InterfaceObservable;
import interfaceDomain.swing.panels.ConsolePanel;
import interfaceDomain.swing.panels.ControlsPanel;
import interfaceDomain.swing.panels.MainPanel;

public class SwingInterface extends Interface {
	
	public JFrame frame;
	
	public MainPanel mainPanel;
	public ConsolePanel consolePanel;
	public ControlsPanel controlsPanel;
	
	public JPanel lower;
	
	public Experiment exp;
	
	public SwingInterface(InterfaceDomain interfaceDomain) {
		super(interfaceDomain);
	}
	
	@Override
	public void initialise() {
		frame = new JFrame("Spock");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		mainPanel = new MainPanel(this);
		consolePanel = new ConsolePanel(this);
		controlsPanel = new ControlsPanel(this);
		
		frame.getContentPane().add(mainPanel);
		
		lower = new JPanel();
		lower.setLayout(new GridLayout(2, 1));
		mainPanel.add(lower, BorderLayout.PAGE_END);
		
		lower.add(consolePanel);
		lower.add(controlsPanel);
		
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
				mainPanel.netPanel.nodes.setText("Nodes: " + (String) arg1);
			} else if (((InterfaceObservable) arg0).getID() == "Node activations") {
				mainPanel.netPanel.activenodes.setText("Node activations: " + (String) arg1);
			} else if (((InterfaceObservable) arg0).getID() == "Latest transfer") {
				consolePanel.console.addElement((String) arg1);
			}
			
		} else {
			Assert.CriticalAssertTrue("Should not happen - observable not InterfaceObservable", false);
		}
	}

}
