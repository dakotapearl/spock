package configurationDomain;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import tools.errorChecking.Log;

import configurationDomain.exceptions.FileAlreadyLoadedException;
import configurationDomain.exceptions.SectionAlreadyExistsException;
import configurationDomain.exceptions.SectionNotFoundException;
import configurationDomain.exceptions.SettingNotFoundException;

import application.Domain;
import application.DomainContainer;

public class ConfigurationDomain extends Domain {

	private HashSet<String> loadedFiles;
	private HashMap<String, ConfigSection> allSettings;
	private HashMap<String, BehaviourConfig> behaviours;
	
	private class ConfigSection {
		public HashMap<String, String> settings;
		@SuppressWarnings("unused")
		public String path;
		public ConfigSection() {
			settings = new HashMap<>();
		}
	}
	
	@SuppressWarnings("unused")
	private class BehaviourConfig {
		
		public String name = null;
		public String path = null;
		
		public String fcPath = null;
		public String tsPath = null;
		public String tcPath = null;
		public String dpPath = null;
		public String eePath = null;
		public String lcPath = null;
		
		public String ipPath = null;
		public String opPath = null;
		public String spPath = null;
		public String gsPath = null;
		
		public boolean isValid() {
			if (name != null && fcPath != null && tsPath != null && tcPath != null && dpPath != null && eePath != null && lcPath != null) {
				return true;
			} else {
				return false;
			}
		}
		
		public boolean filesExist() {
			return false;
			//TODO test each file name passed in
		}
		
	}
	
	public ConfigurationDomain(DomainContainer container) {
		super(container);
	}

	@Override
	public void initialiseIndependent() {
		
		allSettings = new HashMap<String, ConfigSection>();
		loadedFiles = new HashSet<>();
		behaviours = new HashMap<String, BehaviourConfig>();
		
		Log.write("Configuration domain initialised (independent)");
	}
	
	@Override
	public void initialiseInterconnected() {
		Log.write("Configuration domain initialised (interconnected)");
	}
	
	public String getSetting(String section, String name) throws SectionNotFoundException, SettingNotFoundException {
		ConfigSection sectionMap;
		
		if (!allSettings.containsKey(section))
			throw new SectionNotFoundException();
		
		sectionMap = allSettings.get(section);
		
		if (!sectionMap.settings.containsKey(name))
			throw new SettingNotFoundException();
		
		return allSettings.get(section).settings.get(name);
	}
	
	public ConfigSection getSection(String section) throws SectionNotFoundException {
		
		if (!allSettings.containsKey(section))
			throw new SectionNotFoundException();
		
		return allSettings.get(section);
	}
	
	public void setSetting(String section, String name, String value) throws SectionNotFoundException {
		
	}
	
	public void addSection(String section) throws SectionAlreadyExistsException {
		
		if (allSettings.containsKey(section))
			throw new SectionAlreadyExistsException();
		
		allSettings.put(section, new ConfigSection());
		
	}
	
	public void removeSection(String section) throws SectionNotFoundException {
		allSettings.remove(section);
	}
	
	public void saveConfig(String section) {
		
	}
	
	public void loadConfigWithStandardHandling(String filename, String newSection) {
		try {
			loadConfig(filename, newSection);
		} catch (FileNotFoundException e) {
			System.out.println("The settings file for '" + newSection + " was not found (" + filename + ")");
			// TODO create empty config and save
			System.exit(1);
		} catch (XMLStreamException e) {
			System.out.println("An error was found in the settings file for '" + newSection + "' (" + filename + ")");
			System.exit(1);
		} catch (FileAlreadyLoadedException e) {
			// Do nothing, all good
		} catch (SectionAlreadyExistsException e) {
			System.out.println("This configuration section name has already been taken (" + newSection + ")");
			System.exit(1);
		}
	}
	
	public void loadConfig(String filename, String newSection) throws FileAlreadyLoadedException, SectionAlreadyExistsException, XMLStreamException, FileNotFoundException {
		
		filename = "/home/loren/spock/" + filename; // TODO figure out relative pathing
		
		if (loadedFiles.contains(filename))
			throw new FileAlreadyLoadedException();
		
		loadedFiles.add(filename);
		
		XMLInputFactory factory = XMLInputFactory.newInstance();
		XMLEventReader r = factory.createXMLEventReader(filename, new FileInputStream(filename));
		XMLEvent e;
		
		String section = newSection;
		String upSection = newSection;
		ArrayList<String> sectionList = new ArrayList<String>();
		BehaviourConfig newBehaviour = null;
		String behaviourProperty = null;
		
		addSection(newSection);
		
		// Make sure the document has a <configuration> tag as the main tag
		if (r.hasNext()) {
			
			try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
			
			if (!(e.getEventType() == XMLEvent.START_DOCUMENT)) {
				System.out.println("No start document"); // TODO
				return;
			}
			
			try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
			
			if (!(e.getEventType() == XMLEvent.START_ELEMENT && e.toString().equals("<configuration>"))) {
				System.out.println("<configuration> not the main tag: " + e.toString()); // TODO
				return;
			}
			
		}
		
		while (r.hasNext()) {
			
		    try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
		    
		    switch (e.getEventType()) {
		    case XMLEvent.START_ELEMENT:
		    	
		    	if (e.asStartElement().getName().getLocalPart().equals("behaviour")) {
		    		newBehaviour = new BehaviourConfig();
		    	} else if (newBehaviour != null) { // enter behaviour property
		    		behaviourProperty = e.asStartElement().getName().getLocalPart();
		    	} else {
			    	sectionList.add(e.asStartElement().getName().getLocalPart());
			    	upSection = section;
			    	section = section + "." + e.asStartElement().getName().getLocalPart();
		    	}
			    
		    	break;
		    	
		    case XMLEvent.END_ELEMENT:
		    	
		    	if (e.asEndElement().getName().getLocalPart().equals("behaviour")) {
		    		if (newBehaviour.isValid()) {
		    			behaviours.put(newBehaviour.name, newBehaviour);
		    			newBehaviour = null;
		    		} else { // invalid behaviour configuration
		    			if (newBehaviour.name != null) {
		    				System.out.println("Invalid behaviour config discarded: " + newBehaviour.name);
		    			} else {
		    				System.out.println("Invalid behaviour config discarded");
		    			}
		    			newBehaviour = null;
		    		}
		    	} else if (newBehaviour != null) { // leave behaviour property
		    		behaviourProperty = null; 
		    	} else {
			    	if (sectionList.size() > 0) {
			    		section = upSection;
			    		if (sectionList.size() > 1)
			    			upSection = upSection.substring(0, section.length() - sectionList.get(sectionList.size() - 1).length()); 
			    		sectionList.remove(sectionList.size() - 1);
			    	} // TODO Check that this is right
		    	}
		    	break;
		    	
		    case XMLEvent.CHARACTERS:
		    	
		    	// If not white space
		    	if (!(e.isCharacters() && e.asCharacters().isWhiteSpace())) {
		    		
		    		if (!allSettings.containsKey(upSection)) {
		    			//allSettings.put(upSection, new HashMap<String, String>());
		    			System.out.println("Should not happen!");
		    			System.exit(1);
		    		}
		    		
		    		if (newBehaviour != null) { // enter behaviour property
		    			if (behaviourProperty.equals("name")) {
			    			newBehaviour.name = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("path")) {
			    			newBehaviour.path = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("fc")) {
			    			newBehaviour.fcPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("ts")) {
			    			newBehaviour.tsPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("tc")) {
			    			newBehaviour.tcPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("dp")) {
			    			newBehaviour.dpPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("ee")) {
			    			newBehaviour.eePath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("lc")) {
			    			newBehaviour.lcPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("ip")) {
			    			newBehaviour.ipPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("op")) {
			    			newBehaviour.opPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("sp")) {
			    			newBehaviour.spPath = e.asCharacters().getData();
			    		} else if (behaviourProperty.equals("gs")) {
			    			newBehaviour.gsPath = e.asCharacters().getData();
			    		}
			    	} else {
				    	allSettings.get(upSection).settings.put(sectionList.get(sectionList.size() - 1), e.asCharacters().getData());
			    	}
				    
		    	}
		    	
		    	break;
		    	
		    }
		}

	}
		
	// Testing
	public static void main(String[] args) {
		ConfigurationDomain c = new ConfigurationDomain(new DomainContainer());
		
		c.initialiseIndependent();
		
		try {
			c.loadConfig("config/interface.xml", "interface");
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		try {
			ConfigSection cs = c.getSection("interface");
			for (String s : cs.settings.keySet()) {
				System.out.println(s+": "+cs.settings.get(s));
			}
		} catch (SectionNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		try {
			System.out.println(c.getSetting("interface", "type"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		try {
			c.loadConfig("config/network.xml", "network");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileAlreadyLoadedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SectionAlreadyExistsException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		for (String bc : c.behaviours.keySet()) {
			System.out.println(bc + ": " + c.behaviours.get(bc).path);
		}
		
		System.out.println("fc: " + c.behaviours.get("test").path + c.behaviours.get("test").fcPath);
		
		
		
	}

	
	
}
