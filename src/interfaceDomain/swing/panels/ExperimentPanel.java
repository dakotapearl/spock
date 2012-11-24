package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import java.awt.GridLayout;
import java.awt.Panel;

import javax.swing.JLabel;

public class ExperimentPanel extends Panel {

	private static final long serialVersionUID = -3373512759647832446L;
	public JLabel title;
	public JLabel expname;
	SwingInterface swingInterface;
	
	public ExperimentPanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
		setLayout(new GridLayout(5, 1));
		
		title = new JLabel("Experiment          ");
		add(title);
		
		expname = new JLabel("Name: ");
		add(expname);
		
	}
	
}
