package spockdataaccess.entity;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "UserInterfaces")
public class UserInterface implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    protected String Type;
    protected String IPAddress;
    @ManyToOne
    protected User user;
    @ManyToOne
    protected Experiment experiment;

    public UserInterface() {
    }

    public UserInterface(String Type, String IPAddress, User user, Experiment experiment) {
        this.Type = Type;
        this.IPAddress = IPAddress;
        this.user = user;
        this.experiment = experiment;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getType() {
        return Type;
    }

    public void setType(String Type) {
        this.Type = Type;
    }

    public String getIPAddress() {
        return IPAddress;
    }
    
    public void setIPAddress(String IPAddress) {
        this.IPAddress = IPAddress;
    }
    
    public User getUser() {
        return user;
    }

    public Experiment getExperiment() {
        return experiment;
    }

    public void setExperiment(Experiment experiment) {
        this.experiment = experiment;
    }

    public void setUser(User user) {
        this.user = user;
    }
    
    
    
    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof UserInterface)) {
            return false;
        }
        UserInterface other = (UserInterface) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.UserInterface[ id=" + id + " ]";
    }
    
}
